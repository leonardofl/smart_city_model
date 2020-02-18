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
-define( wooper_construct_parameters, ActorSettings, BikeName , Trips , StartTime , Type , Mode ).

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
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2 ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a bike that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, BikeName ),

	InitialTrip = lists:nth( 1 , Trips ), 	
	Path = element( 2 , InitialTrip ), 

	InitialState = setAttributes( ActorState, [
		{ bike_name, BikeName },
		{ trips , Trips },
		{ type, Type },
		{ distance , 0 }, 
		{ position, -1 },
		{ start_time , StartTime },
		{ path , Path },
		{ mode , Mode },
		{ last_vertex , ok },
		{ last_vertex_pid , ok }]
	),
    InitialState.

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	State.






-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->
	% TODO: Why this is needed?
	StartTime = getAttribute( State , start_time ),
    FirstActionTime = class_Actor:get_current_tick_offset( State ) + StartTime,   	
	NewState = setAttribute( State , start_time , FirstActionTime ),
	executeOneway( NewState , addSpontaneousTick , FirstActionTime ).







-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->	
	Trips = getAttribute( State , trips ), 
	Path = getAttribute( State , path ), 
	verify_next_action( State , Trips , Path ).

verify_next_action( State , _Trips , Path ) when Path == false ->
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
	CarId = getAttribute( State , car_name ),	
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


request_position( State , Trip , Path ) when length( Path ) > 1 ->
	get_next_vertex( State , Path );

request_position( State , Trip , Path ) when length( Path ) == 1 ->
	FinalState = setAttribute( State, path , finish ),
	executeOneway( FinalState , scheduleNextSpontaneousTick ).

% [ Current | Path ]: Current é 1o elemento da lista e Path é uma lista com o restante dos elementos
get_next_vertex( State , [ Current | Path ] )  -> % baseado no Mode == walk do class_Car			
	Vertices = list_to_atom( lists:concat( [ Current , lists:nth( 1 , Path ) ] )),
	
	LinkData = lists:nth( 1, ets:lookup( list_streets , Vertices ) ),
    % LinkData: {_, Id, Length, RawCapacity, Freespeed, NumberCars, Lanes, {}}
    {_, Id, Length, Capacity, _Freespeed, NumberCars, _Lanes, _DR} = LinkData,
    Speed = traffic_models:get_speed_bike(), % TODO passar parâmetros LinkData
    Time = (Length / Speed) + 1,

    Distance = round(Length),
	TotalLength = getAttribute( State , distance ) + Distance,
	FinalState = setAttributes( State , [ { distance , TotalLength } , { bike_position , Id } , { path , Path } ] ), 

	%print_movement( State ),

	executeOneway( FinalState , addSpontaneousTick , class_Actor:get_current_tick_offset( FinalState ) + Time ).




