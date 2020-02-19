-module(traffic_models).

-export([get_speed_car/2, get_speed_walk/2, get_speed_bike/9, get_personal_bike_speed/0]).

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
    FactorOdToBikeSampa = 10/7.5,
    SampledSpeed * FactorOdToBikeSampa.

get_next_value_from_speeds_distribution() ->

    % First, we open the file if it's the first time this function is invoked
    case lists:member(table_personal_speeds, ets:all()) of
        false -> 
            ets:new(table_personal_speeds, [named_table, protected, set, {keypos, 1}]),
            Filename = "personal_speed_distribution.csv",
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
% Length: length of the link (meters)
% Capacity: how many cars the link supports
% NumberBikes: how many bikes are in the link
% NumberCars: how many cars are in the link
% IsCycleway: boolean
% IsCyclelane: boolean
% AltitudeNodeFrom: altitude of the origin node (meters)
% AltitudeNodeTo: altitude of the target node (meters)
get_speed_bike(PersonalSpeed, Length, Capacity, NumberCars, NumberBikes, IsCycleway, IsCyclelane, AltitudeNodeFrom, AltitudeNodeTo) ->
    Freespeed = get_free_speed_for_bike(PersonalSpeed, Length, IsCycleway, IsCyclelane, AltitudeNodeTo, AltitudeNodeFrom),
    Speed = speed_for_bike_considering_traffic(Freespeed, Capacity, NumberCars, NumberBikes, IsCycleway, IsCyclelane),
    Speed.



get_free_speed_for_bike(PersonalSpeed, Length, IsCycleway, IsCyclelane, AltitudeNodeTo, AltitudeNodeFrom) ->

    Inclination = (AltitudeNodeTo - AltitudeNodeFrom) / Length,
    Climb = Inclination > 0.02,
    Descent = Inclination < -0.02,
%    IsMixedTraffic = (not IsCycleway) and (not IsCyclelane),
%    Plane = (not Climb) and (not Descent),

    % Base for the factors (Bike Sampa dataset):
    % Plane cyclelane: 9.6km/h
    % Climb cyclelane: 6.8km/h
    % Descent cyclelane: 15.1km/h
    % Plane Cycleway: 15.5km/h
    CyclelaneFactor = 1.2, % at Bike Sampa, mean speed is 9.6km/h
    CyclewayFactor = 1.5, % at Bike Sampa, mean speed is 15.5km/h
    ClimbFactor = 0.71,
    DescentFactor = 1.57,

    SpeedWithInfra = if
        IsCyclelane -> PersonalSpeed * CyclelaneFactor;
        IsCycleway -> PersonalSpeed * CyclewayFactor;
        true -> PersonalSpeed
    end,

    FreeSpeed = if
        Climb -> SpeedWithInfra * ClimbFactor;
        Descent -> SpeedWithInfra * DescentFactor;
        true -> SpeedWithInfra
    end,

    FreeSpeed.
   







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

