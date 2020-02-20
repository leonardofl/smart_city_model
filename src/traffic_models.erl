-module(traffic_models).

-export([get_speed_car/2, get_speed_walk/2, get_speed_bike/7, get_personal_bike_speed/0]).

% There is DR in link and car can use it:
get_speed_car({_, Id, Length, RawCapacity, Freespeed, NumberCars, Lanes, {_DRName, _DigitalRailsLanes, _Cycle, _Bandwidth, _Signalized, _Offset}}, true) ->
	link_density_speed(Id, Length, RawCapacity, NumberCars, Freespeed, Lanes);

% There is DR but not effective:
get_speed_car({Whatever, Id, Length, RawCapacity, Freespeed, NumberCars, Lanes, {_DRName, _DigitalRailsLanes, _Cycle, _Bandwidth, _Signalized, _Offset}}, noeffect) ->
	get_speed_car({Whatever, Id, Length, RawCapacity, Freespeed, NumberCars, Lanes, {}}, noeffect);

% There is DR but car cannot use it:
get_speed_car({_, Id, Length, RawCapacity, Freespeed, NumberCars, Lanes, {_DRName, _DigitalRailsLanes, _Cycle, _Bandwidth, _Signalized, _Offset}}, false) ->
	link_density_speed(Id, Length, RawCapacity, NumberCars, Freespeed, Lanes);

% There is no DR:
get_speed_car({_, Id, Length, RawCapacity, Freespeed, NumberCars, Lanes, {}}, _) ->
	link_density_speed(Id, Length, RawCapacity, NumberCars, Freespeed, Lanes).

link_density_speed(Id, Length, Capacity, NumberCars, Freespeed, _Lanes) ->

	Alpha = 1,
	Beta = 1,
	Speed = case NumberCars >= Capacity of
		true -> 1.0;
		false -> Freespeed * math:pow(1 - math:pow((NumberCars / Capacity), Beta), Alpha)
	end,

	Time = (Length / Speed) + 1,
	{Id, round(Time), round(Length)}.




get_speed_walk(LinkData, _) ->
	{_, Id, Length, _, _, _} = LinkData,	
	Time = ( Length / 2 ),

	{Id, ceil(Time), round(Length)}.



%%%%%%%%%%%%%%% BIKES %%%%%%%%%%%%%%%%%%%%%%%

get_personal_bike_speed() ->
    % The sampled speed is retrieved from a previously generated list following a distribution observed in the OD (Pesquisa OrigemDestino) data.
    SampledSpeed = get_next_value_from_speeds_distribution(),
    % What matters in this speeds list is the distribution (a generalized gama, as told us by the fit test done in R).
    % But the speeds in our OD dataset are lower than actual speeds, because we used as distance the euclidean distance from origin to destination - a straight line linking origin and destination. 
    % The mean speed for the OD dataset is 7.5km/h.
    % We considered the Bike Sampa dataset to me more accurate. In this Bike Sampa dataset the mean speed mixed traffic is 10km/h.
    % Therefore, we apply a factor over the sampled speed to correct the offset of the speed distribution.
    FactorOdToBikeSampa = 10/7.44,
    SampledSpeed * FactorOdToBikeSampa.

get_next_value_from_speeds_distribution() ->

    % First, we open the file if it's the first time this function is invoked
    case lists:member(table_personal_speeds, ets:all()) of
        false -> 
            ets:new(table_personal_speeds, [named_table, protected, set, {keypos, 1}]),
            Filename = "personal_speed_distribution_for_bikes.csv",
            {_ok, File} = file:open(Filename, read),
            ets:insert(table_personal_speeds, {file, File});
            % we keep the file in the EST, so we keep implicitly together the pointer to the last read line.
        _ -> nothing_to_do
    end,

    [{_, SpeedsFile}] = ets:lookup(table_personal_speeds, file),
    Line = io:get_line(SpeedsFile, ''), % read next line of the file
    % Obs: if we got at the end of file, the system will crash! But we hope this to not happen!
    {SpeedKmh, _} = string:to_float(Line),
    SpeedKmh/3.6.

% PersonalSpeed: different people have different speeds; each agent must hold a personal speed generated once for the actor. The personal speed must be generated using the function get_personal_bike_speed.
% Capacity: how many cars the link supports
% NumberBikes: how many bikes are in the link
% NumberCars: how many cars are in the link
% IsCycleway: boolean
% IsCyclelane: boolean
% Inclination: (altitude_to - altitude_from) / length 
get_speed_bike(PersonalSpeed, Capacity, NumberCars, NumberBikes, IsCycleway, IsCyclelane, Inclination) ->
    Freespeed = get_free_speed_for_bike(PersonalSpeed, IsCycleway, IsCyclelane, Inclination),
    Speed = speed_for_bike_considering_traffic(Freespeed, Capacity, NumberCars, NumberBikes, IsCycleway, IsCyclelane),
    Speed.



get_free_speed_for_bike(PersonalSpeed, IsCycleway, IsCyclelane, Inclination) ->

    Climb = Inclination > 0.02,
    Descent = Inclination < -0.02,
    IsMixedTraffic = (not IsCycleway) and (not IsCyclelane),
    Plane = (not Climb) and (not Descent),

    if 
        Plane and IsCycleway ->
            PersonalSpeed * 1.4651;
        Plane and IsCyclelane ->
            PersonalSpeed * 0.5524;
        Plane and IsMixedTraffic ->
            PersonalSpeed * 1.5;
        Climb and (IsCycleway or IsCyclelane) ->
            PersonalSpeed * 0.3827;
        Climb and IsMixedTraffic ->
            PersonalSpeed * 0.8027;
        Descent and (IsCycleway or IsCyclelane) ->
            PersonalSpeed * 1.4532;
        Descent and IsMixedTraffic ->
            PersonalSpeed * 1.1217;
        true ->
            erlang:error("It should never happen, unexpected combination of arguments when calculating bike speed: IsCycleway=" ++ IsCycleway ++ ", IsCyclelane=" ++ IsCyclelane ++ ", Inclination=" ++ Inclination)
    end.
   







speed_for_bike_considering_traffic(BaseSpeed, Capacity, NumberCars, NumberBikes, IsCycleway, IsCyclelane) ->

    IsMixedTraffic = (not IsCycleway) and (not IsCyclelane),
    Occupation = if
        IsMixedTraffic ->
            % cellsize do carro = 7,5 e cellsize da bike = 3
            % cellsize é o comprimento do veículo mais a distância de segurança
            % Considera também que em uma faixa de carro passam duas bikes uma do lado da outra 
            NumberBikes/(2.5*2) + NumberCars;
        true -> % ciclovia ou ciclofaixa (é uma bike atrás da outra)
            NumberBikes/(2.5*1)
    end,
    SaturatedLink = Occupation >= Capacity,

	Alpha = 1,
	Beta = 1,
    if
        SaturatedLink ->
             1.0;
        true -> % (não saturado) ou (saturado em tráfego misto)
            BaseSpeed * math:pow(1 - math:pow((Occupation / Capacity), Beta), Alpha)
	end.

