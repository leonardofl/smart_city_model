-module(class_Bike).
% based on class_Car

% "Teste" de class_Bike é a compilação.
% Primeiro roda "erlc class_Bike.erl" pra antecipar erros de sintaxe.
% Aí roda "docker build -t interscsimulator .", que vai tentar compilar class_Bike
% e dar erro se não conseguir.

-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
% Trips: list of trips since the trip can be a multi-trip
%        each trip is a tuple { Mode , Path , LinkOrigin }
%        LinkOrigin is not used here; Mode is also not used, since here we assume mode is always bike 
% Type: reason for the trip; not used
% Mode: bike
-define( wooper_construct_parameters, ActorSettings, BikeName , ListTripsFinal , StartTime , Type , Park , Mode, DigitalRailsCapable ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/8, new_link/8,
		 synchronous_new/8, synchronous_new_link/8,
		 synchronous_timed_new/8, synchronous_timed_new_link/8,
		 remote_new/9, remote_new_link/9, remote_synchronous_new/9,
		 remote_synchronous_new_link/9, remote_synchronisable_new_link/9,
		 remote_synchronous_timed_new/9, remote_synchronous_timed_new_link/9,
		 construct/9, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, get_parking_spot/3 , receive_signal_state/3 ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() , parameter(), parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, BikeName ),

	InitialTrip = lists:nth( 1 , ListTripsFinal ),	
	Path = element( 2 , InitialTrip ),

	NewState = setAttributes( ActorState, [
		{ bike_name, BikeName },
		{ trips , ListTripsFinal },
		{ type, Type },
		{ distance , 0 },
		{ car_position, -1 },
		{ start_time , StartTime },
		{ path , Path },
		{ mode , Mode },
		{ last_vertex , ok },
		{ last_vertex_pid , ok },
		{ previous_dr_name, nil },
		{ in_platoon, false },
		{ digital_rails_capable, DigitalRailsCapable}] 
	),

	case Park of
		ok ->
			setAttribute( NewState , park_status , not_parking );
		_ ->
			setAttributes( NewState , [ { park_status , find } , { park , Park } ] )
	end.

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->	
	Trips = getAttribute( State , trips ), 
	Path = getAttribute( State , path ), 
	verify_next_action( State , Trips , Path ).

verify_next_action( State , _Trip , Path ) when Path == false ->
	executeOneway( State , declareTermination );

verify_next_action( State , Trips , Path ) when length( Trips ) == 0, Path == finish -> 
	executeOneway( State , declareTermination );

verify_next_action( State , Trips , Path ) when length( Trips ) > 0 ->
	CurrentTrip = lists:nth( 1 , Trips ),		
	?wooper_return_state_only( request_position( State , CurrentTrip , Path ) );

verify_next_action( State , _Trips , _Path ) ->
	Type = getAttribute( State , type ),						
	TotalLength = getAttribute( State , distance ),
	StartTime = getAttribute( State , start_time ),
	CarId = getAttribute( State , bike_name ),	
	LastPosition = getAttribute( State , car_position ),
	Mode = getAttribute( State , mode ), 

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	print:write_final_message( Type , TotalLength , StartTime , CarId , CurrentTickOffset , LastPosition , Mode , csv ),
	PathFinish = setAttribute( State , path , finish ),

	executeOneway( PathFinish , scheduleNextSpontaneousTick ).

request_position( State , _Trip , Path ) when Path == finish ->
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),
	Trips = getAttribute( State , trips ), 
	NewTrips = list_utils:remove_element_at( Trips , 1 ),
	
	NewState = case length( NewTrips ) > 0 of
		true -> 
			InitialTrip = lists:nth( 1 , NewTrips ),	
			NewPath = element( 2 , InitialTrip ),
			setAttributes( State , [ { trips , NewTrips } , { path, NewPath} ] );
		false -> 
			setAttributes( State , [ { trips , NewTrips } , { path, ok} ] )
	end,
	executeOneway( NewState , addSpontaneousTick , CurrentTickOffset + 1 );	


request_position( State , Trip , Path ) ->
	case length( Path ) > 1 of
		true ->	get_next_vertex( State , Path , element( 1 , Trip ) );
		false -> verify_park( State , element( 1 , Trip ) )
	end.

verify_park( State , Mode ) when Mode == walk ->
	FinalState = setAttribute( State, path , finish ),
	executeOneway( FinalState , scheduleNextSpontaneousTick );


verify_park( State , _Mode ) ->						
	DecrementVertex = getAttribute( State , last_vertex_pid ),	
	ets:update_counter( list_streets , DecrementVertex , { 6 , -1 }),
	ParkStatus = getAttribute( State , park_status ),

	case ParkStatus of

		not_parking ->
			FinalState = setAttribute( State , path , finish ),

			executeOneway( FinalState , scheduleNextSpontaneousTick );
		finish ->

			Park = getAttribute( State , park ),
					
			Parking = ets:lookup_element(options, parking_pid, 2 ),
			NewState = class_Actor:send_actor_message( Parking, { spot_in_use, { Park } } , State ),
									
			FinalState = setAttribute( NewState, path , finish ),

			executeOneway( FinalState , scheduleNextSpontaneousTick );
		find ->
			Park = getAttribute( State , park ),
			Parking = ets:lookup_element(options, parking_pid, 2 ),
			class_Actor:send_actor_message( Parking, { spot_available, { Park } } , State )
	end.

is_changing_dr(State, {Name, _DRLanes, _Cycle, _Bandwidth, _, _}) ->
	case getAttribute(State, digital_rails_capable) of 
		true -> 
			case getAttribute(State, previous_dr_name) of
				Name -> false;
				_ -> true
			end;
		_ -> false
	end;

is_changing_dr(_, _) -> false.

get_next_vertex( State , [ Current | Path ] , Mode ) when Mode == walk ->			
	Vertices = list_to_atom( lists:concat( [ Current , lists:nth( 1 , Path ) ] )),
	
	Data = lists:nth( 1, ets:lookup( list_streets , Vertices ) ),
	{ Id , Time , Distance } = traffic_models:get_speed_walk(Data, getAttribute(State, traffic_model)),

	TotalLength = getAttribute( State , distance ) + Distance,
	FinalState = setAttributes( State , [ { distance , TotalLength } , { car_position , Id } , { path , Path } ] ), 

	%print_movement( State ),

	executeOneway( FinalState , addSpontaneousTick , class_Actor:get_current_tick_offset( FinalState ) + Time );

get_next_vertex( State, [ _CurrentVertex | _ ], _Mode) -> 
	LastVertex = getAttribute(State, last_vertex),
	[CurrentVertex | [ NextVertex | _ ]] = getAttribute(State, path),
	% [ CurrentVertex | _ ] = getAttribute( State , path ),
	Edge = list_to_atom(lists:concat([CurrentVertex, NextVertex])),

	LinkData = lists:nth(1, ets:lookup(list_streets, Edge)),
	{_, _, _, _, _, _, _Lanes, DigitalRailsInfo} = LinkData,

	_ChangingDR = is_changing_dr(State, DigitalRailsInfo),
%	case ChangingDR of 
%		true ->
%			{Name, _DRLanes, Cycle, Bandwidth, _, _} = DigitalRailsInfo,
%			T = round(rand:uniform() * Cycle),
%			StateAfter = setAttributes(State, [{previous_dr_name, Name}]),
%		 	case T > Bandwidth of
%				true -> executeOneway(StateAfter, addSpontaneousTick, class_Actor:get_current_tick_offset(StateAfter) + T);
%				_ -> move_to_next_vertex(StateAfter)
%			end;
%		false -> move_to_next_vertex(State)
%	end.
	
	% Current vertex is an atom here, but at the ets it is a string. Must convert:
	CurrentVertexStr = lists:flatten(io_lib:format("~s", [CurrentVertex])),
	Matches = ets:lookup(traffic_signals, CurrentVertexStr),

	case length(Matches) of
		0 -> move_to_next_vertex(State);
	 	_ -> 	
			case LastVertex of
				ok -> move_to_next_vertex(State);
				_ ->
					{_, TrafficSignalsPid} = lists:nth(1, Matches),
					class_Actor:send_actor_message(TrafficSignalsPid, {querySignalState, LastVertex}, State)
			end
	 end.
	%move_to_next_vertex(State).

move_to_next_vertex( State ) ->
	[ CurrentVertex | [ NextVertex | Path ] ] = getAttribute( State , path ),
	Edge = list_to_atom(lists:concat([ CurrentVertex , NextVertex ])),

	DecrementVertex = getAttribute( State , last_vertex_pid ),
	Mode = getAttribute( State , mode ), 
	case Mode of
		platoon -> 
	%		io:format( "platoon" ),
			ets:update_counter(drs_streets, Edge , { 2 , 1 } ),
			case DecrementVertex of
				ok -> ok;
				_ -> ets:update_counter( drs_streets, DecrementVertex , { 2 , -1 } )
			end;
	 	_ -> ok
	end,
		
	{ Data, NewState } = case getAttribute(State, digital_rails_capable) of
		true ->  
			NewNewNewState = case Mode of
				platoon -> 
					case DecrementVertex of
						ok -> ok;
						_  -> ets:update_counter( list_streets_dr, DecrementVertex , { 6 , -1 })
					end,	
					ets:update_counter( list_streets_dr , Edge , { 6 , 1 }),
					State;
				car ->
					IsPlatoon = getAttribute( State, in_platoon ),
					NewNewState = case IsPlatoon of
						true ->	
							State;				
						false -> 
							case DecrementVertex of
								ok -> ok;
								_  -> ets:update_counter( list_streets_dr, DecrementVertex , { 6 , -1 })
							end,	
							StreetDR = ets:lookup( drs_streets, Edge ),
							HasDR = lists:nth(1, StreetDR),
							StatePlatoon = case element( 2 , HasDR ) > 0 of
								true -> 
									setAttribute( State , in_platoon , true );
								false -> 
									ets:update_counter( list_streets_dr , Edge , { 6 , 1 }),
									State
							end,
							StatePlatoon
					end,	
					NewNewState
			end,
			DataReturn = lists:nth(1, ets:lookup(list_streets_dr , Edge)),
			{ DataReturn , NewNewNewState };
		_ -> 
			case DecrementVertex of
				ok -> ok;
				_ -> ets:update_counter( list_streets, DecrementVertex , { 6 , -1 })
			end,	
			ets:update_counter( list_streets , Edge , { 6 , 1 }),
			DataReturn = lists:nth(1, ets:lookup(list_streets , Edge)),
			{DataReturn , State }
	end,

	{ Id , Time , Distance } = traffic_models:get_speed_car(Data, getAttribute(NewState, digital_rails_capable)),

	TotalLength = getAttribute( NewState , distance ) + Distance,
	StateAfterMovement = setAttributes( NewState , [
		{distance , TotalLength} , {car_position , Id} , {last_vertex, CurrentVertex}, {last_vertex_pid , Edge} , {path , [NextVertex | Path]}] ), 

	% io:format("t=~p: ~p; ~p->~p ~n", [class_Actor:get_current_tick_offset(State), getAttribute(State, bike_name), CurrentVertex, NextVertex]),
	% io:format("~p Tick: ~p; ~p => ~p, Dist: ~p, Time: ~p, Avg. Speed: ~p, NextTick: ~p\n", 
	% 	[getAttribute( State , bike_name ), class_Actor:get_current_tick_offset( State ), CurrentVertex, NextVertex, Distance, Time, Distance / Time, class_Actor:get_current_tick_offset( StateAfterMovement ) + Time]),

%	print_movement(State, StateAfterMovement),
	executeOneway( StateAfterMovement , addSpontaneousTick , class_Actor:get_current_tick_offset( StateAfterMovement ) + Time ).

-spec receive_signal_state(wooper:state(), tuple(), pid()) -> oneway_return().
receive_signal_state( State , {Color, TicksUntilNextColor}, _TrafficLightPid ) -> 
	case Color of
		red -> 
			% io:format("[~p] red (green in ~p)\n", [TrafficLightPid, TicksUntilNextColor]),
			% Act spontaneously when the traffic light is green again...
			executeOneway( State , addSpontaneousTick , class_Actor:get_current_tick_offset( State ) + TicksUntilNextColor );
		green -> 
			% io:format("Traffic signal is green, continuing movement...\n"),
			move_to_next_vertex(State)
	end.

get_parking_spot( State , IdNode , _ParkingPID ) ->
	Node = element( 1 , IdNode ),
	case Node of 
	     nok ->
		io:format( "nao disponivel");
    	     _ ->
		{ Path , City } = { getAttribute( State , path ), ets:lookup_element(options, city_pid, 2 ) },
		CurrentVertice = lists:nth( 1 , Path ),
		class_Actor:send_actor_message( City , { get_path, { CurrentVertice , Node } } , State )
	end.
 

-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->
	% TODO: Why this is needed?
	StartTime = getAttribute( State , start_time ),
    	FirstActionTime = class_Actor:get_current_tick_offset( State ) + StartTime,   	
	NewState = setAttribute( State , start_time , FirstActionTime ),
	executeOneway( NewState , addSpontaneousTick , FirstActionTime ).

