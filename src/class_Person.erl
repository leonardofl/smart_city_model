%Class that represents a person that can moves around the city graph on foot or by car
-module(class_Person).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName, ListVertex , ListTripsFinal , StartTime , LogPID , Type , Mode , PID ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/9, new_link/9,
		 synchronous_new/9, synchronous_new_link/9,
		 synchronous_timed_new/9, synchronous_timed_new_link/9,
		 remote_new/10, remote_new_link/10, remote_synchronous_new/10,
		 remote_synchronous_new_link/10, remote_synchronisable_new_link/10,
		 remote_synchronous_timed_new/10, remote_synchronous_timed_new_link/10,
		 construct/10, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, go/3 , metro_go/3 , bus_go/3 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() , parameter() , parameter() , parameter()  ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, CarName ),

        DictVertices = dict:from_list( ListVertex ),

	setAttributes( ActorState, [
		{ car_name, CarName },
		{ dict , DictVertices },
		{ trips , ListTripsFinal },
		{ log_pid, LogPID },
		{ type, Type },
		{ distance , 0 },
		{ car_position, -1 },
		{ start_time , StartTime },
		{ path , ok },
		{ cost , 3.8 },
		{ metro , element ( 1 , PID ) },
		{ mode , Mode },
		{ pt_status , start } %public transport -> bus or metro
						] ).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->
	
	Trips = getAttribute( State , trips ), 
	
	case length( Trips ) > 0 of

		false ->		
			
			Path = getAttribute( State , path ), 

			case Path of 

				finish -> 
					
					executeOneway( State , declareTermination );

				_ ->

					NewState = setAttribute( State , path , finish ),

					FinalState = write_final_message( NewState ),

					executeOneway( FinalState, scheduleNextSpontaneousTick )

				end;


		true ->

			CurrentTrip = list_utils:get_element_at( Trips , 1 ),

			Mode = element( 1 , CurrentTrip ),			

			case Mode of 

				"walk" ->
	
					NewState = request_position( State , CurrentTrip  ),
					?wooper_return_state_only( NewState );

				"bus" ->

					PtStatus = getAttribute( State , pt_status ), 

					case PtStatus of 
	
	
						finish ->


							NewTrips = list_utils:remove_element_at( Trips , 1 ),

							NewState = setAttributes( State , [ {trips , NewTrips } , {  pt_status , start } ] ),					

							executeOneway( NewState , scheduleNextSpontaneousTick );

						start ->
								
							NewState = request_position_bus( State , CurrentTrip ),

							?wooper_return_state_only( NewState )

					end;


				"metro" ->
			
					PtStatus = getAttribute( State , pt_status ), 

					case PtStatus of 
	
	
						finish ->
							
							NewTrips = list_utils:remove_element_at( Trips , 1 ),

							NewState = setAttributes( State , [ {trips , NewTrips } , {  pt_status , start } ] ),					
							executeOneway( NewState , scheduleNextSpontaneousTick );

						start ->
	
							NewState = request_position_metro( State , CurrentTrip ),

							?wooper_return_state_only( NewState )

					end

					


			end


	end.

-spec request_position_bus( wooper:state() , parameter() ) -> wooper:state().
request_position_bus( State , Trip ) -> 

	Origin = element( 2 , Trip ),

	Destination = element( 3 , Trip ), 

	Line = element( 4 , Trip ), 

	DictVertices = getAttribute( State , dict ),

	VertexPID = element( 2 , dict:find( list_to_atom( Origin ) , DictVertices) ),	% get the pid of the bus stop vertex

	class_Actor:send_actor_message( VertexPID ,
		{ wait_bus , { Destination , Line } }, State ).

			
-spec request_position_metro( wooper:state() , parameter() ) -> wooper:state().
request_position_metro( State , Trip ) -> 

	Origin = element( 2 , Trip ),

	Destination = element( 4 , Trip ), 

	class_Actor:send_actor_message( ?getAttr( metro ) ,
		{ getTravelTime, { Origin , Destination } }, State ).



-spec metro_go( wooper:state(), value(), pid() ) -> class_Actor:actor_oneway_return().
metro_go( State, PositionTime , _GraphPID ) ->

	TotalTime = class_Actor:get_current_tick_offset( State ) + element( 1 , PositionTime ), % CurrentTime + Time to pass the link

	Trips = getAttribute( State , trips ), 
	
	Trip = list_utils:get_element_at( Trips , 1 ),

	Destination = element( 5 , Trip ), 

	PositionState = setAttributes( State , [ { car_position, list_to_atom( Destination ) } , { pt_status , finish } ] ),

	% FinalState = write_movement_metro_message( StatusState , CurrentTickOffset , Destination ),

	executeOneway( PositionState , addSpontaneousTick, TotalTime ).

-spec bus_go( wooper:state(), value(), pid() ) -> class_Actor:actor_oneway_return().
bus_go( State, _PositionTime , _GraphPID ) ->

	% get the current time of the simulation
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	Trips = getAttribute( State , trips ), 
	
	Trip = list_utils:get_element_at( Trips , 1 ),

	Destination = element( 6 , Trip ), 

	PositionState = setAttributes( State , [ { car_position, list_to_atom( Destination ) } , { pt_status , finish } ] ),

	%FinalState = write_movement_bus_message( StatusState , CurrentTickOffset , CarId , LastPosition , Destination , CarId , Type ),

	executeOneway( PositionState , addSpontaneousTick, CurrentTickOffset + 1 ).



-spec request_position( wooper:state() , parameter() ) -> wooper:state().
request_position( State , Trip ) ->
	
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 	

	PathTest = getAttribute( State , path ),

	PathState = case PathTest of

		ok -> 
			
			PathTrip = element( 5 , Trip ),
			setAttribute( State, path, PathTrip );

		_ ->

			State

	end,

			
	Path = getAttribute( PathState , path ),

	case Path of 

		finish ->

			Trips = getAttribute( State , trips ), 
			
			NewTrips = list_utils:remove_element_at( Trips , 1 ),

			NewState = setAttributes( State , [ { trips , NewTrips } , { path, ok } ] ),

			executeOneway( NewState , addSpontaneousTick , CurrentTickOffset + 1 );	
	
		false ->

			State;

		_ ->

			case length( Path ) > 1 of

				true ->	

					% get the current and the next vertex in the path	
					InitialVertice = list_utils:get_element_at( Path , 1 ),

					FinalVertice = list_utils:get_element_at( Path , 2 ),

					DictVertices = getAttribute( PathState , dict ),

					VertexPID = dict:find( InitialVertice , DictVertices),
					
					Vertices = list_to_atom( lists:concat( [ InitialVertice , FinalVertice ] )),

					FinalState = setAttribute( PathState , path, list_utils:remove_element_at( Path , 1 ) ), % remove the current element of the path
			
					class_Actor:send_actor_message( element( 2 , VertexPID ) ,
						{ getSpeedWalk, { Vertices } }, FinalState );

				false ->							

					LastPosition = getAttribute( PathState , car_position ),

					case LastPosition == -1 of

						true ->
							
							executeOneway( PathState , declareTermination );	

						false ->	

						
							FinalState = setAttribute( PathState, path, finish ),

							executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + 1 )
						
					end

			end

	end.

-spec go( wooper:state(), car_position() , parameter() ) -> class_Actor:actor_oneway_return().
go( State, PositionTime , _GraphPID ) ->

	TotalTime = class_Actor:get_current_tick_offset( State ) + element( 2 , PositionTime ), % CurrentTime + Time to pass the link

	% Calculate the total distance that the person moved until now.
	TotalLength = getAttribute( State , distance ) + element( 3 , PositionTime),
	LengthState = setAttributes( State,  [ { distance , TotalLength } , { car_position , element( 1 , PositionTime ) } ] ),
		
%	TripIndex = getAttribute( State , trip_index ), 

%	Trips = getAttribute( State , trips ), 
	
%	CurrentTrip = list_utils:get_element_at( Trips , TripIndex ),

	%FinalState = case LastPosition == -1 of

	%	false ->
			
	%		write_movement_car_message( NewState , CurrentTickOffset , CarId , LastPosition , NewPosition , Type );


	%	true -> 

	%		LinkOrigin = element( 3 , CurrentTrip ),

	%		write_initial_message( NewState , CurrentTickOffset , CarId , LinkOrigin , NewPosition , Type )
	   


	%end,

	executeOneway( LengthState , addSpontaneousTick , TotalTime ).


% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

    	FirstActionTime = class_Actor:get_current_tick_offset( State ) + getAttribute( State, start_time ),   	

	executeOneway( State , addSpontaneousTick , FirstActionTime ).



write_final_message( State ) ->

	Type = getAttribute( State , type ),
						
	TotalLength = getAttribute( State , distance ),

	StartTime = getAttribute( State , start_time ),

	CarId = getAttribute( State , car_name ),	

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	TotalTime =   CurrentTickOffset - StartTime, 	

	LastPosition = getAttribute( State , car_position ),

	Cost = getAttribute( State , cost ), 

	Mode = getAttribute( State , mode ), 

%	LeavesTraffic = io_lib:format( "<event time=\"~w\" type=\"vehicle leaves traffic\" person=\"~s\" link=\"~s\" vehicle=\"~s\" relativePosition=\"1.0\" />\n", [ CurrentTickOffset , CarId , LastPosition , CarId ] ),
			
%	LeavesVehicles = io_lib:format( "<event time=\"~w\" type=\"PersonLeavesVehicle\" person=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , CarId , CarId ] ),
						
	Arrival = io_lib:format( "<event time=\"~w\" type=\"arrival\" person=\"~s\" vehicle=\"~s\" link=\"~s\" legMode=\"~s\" trip_time=\"~w\" distance=\"~w\" cost=\"~w\" action=\"~s\"/>\n", [ CurrentTickOffset , CarId , CarId ,  LastPosition, Mode , TotalTime , TotalLength , Cost , Type ] ),

%	ActStart = io_lib:format( "<event time=\"~w\" type=\"actstart\" person=\"~s\"  link=\"~s\"  actType=\"h\"  />\n", [ CurrentTickOffset , CarId , LastPosition ] ),

%	TextFile = lists:concat( [ LeavesTraffic , LeavesVehicles , Arrival , ActStart ] ),

	class_Actor:send_actor_message( ?getAttr(log_pid) , { receive_action, { Arrival } }, State ).

%write_initial_message( State , CurrentTickOffset , LinkOrigin , NewPosition  ) ->


  %	CarId = getAttribute( State , car_name ),
  %	Type = getAttribute( State , type ),

 %  	Text1 = io_lib:format( "<event time=\"~w\" type=\"actend\" person=\"~s\" link=\"~s\" actType=\"h\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , Type ] ),
  % 	Text2 = io_lib:format( "<event time=\"~w\" type=\"departure\" person=\"~s\" link=\"~s\" legMode=\"car\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , Type ] ),
  %	Text3 = io_lib:format( "<event time=\"~w\" type=\"PersonEntersVehicle\" person=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , CarId , Type ] ),
  %	Text4 = io_lib:format( "<event time=\"~w\" type=\"wait2link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , CarId , Type ] ),
  	
%	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId , Type ] ),

%	TextFile = lists:concat( [ Text1 , Text2 , Text3 , Text4 , NextPositionText  ] ),

%	LogPID = ?getAttr(log_pid),
		
%	class_Actor:send_actor_message( LogPID, { receive_action, { TextFile } }, State ).


%write_movement_car_message( State , CurrentTickOffset , NewPosition ) ->


  	%CarId = getAttribute( State , car_name ),
  	%Type = getAttribute( State , type ),

	%LastPosition = getAttribute( State , car_position ),	

%	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , atom_to_list(LastPosition) , CarId , Type ] ),
%	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId , Type ] ),

%	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

%	LogPID = ?getAttr(log_pid),

%	class_Actor:send_actor_message( LogPID,	{ receive_action, { TextFile } }, State ).

%write_movement_bus_message( State , CurrentTickOffset ,  Destination ) ->


%	LastPosition = getAttribute( State , car_position ),
 % 	CarId = getAttribute( State , car_name ),
 % 	Type = getAttribute( State , type ),

%	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"bus\" />\n", [ CurrentTickOffset , CarId , LastPosition , CarId , Type ] ),
%	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"bus\" />\n", [  CurrentTickOffset , CarId , Destination , CarId , Type ] ),


%	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

%	LogPID = ?getAttr(log_pid),

%	class_Actor:send_actor_message( LogPID, { receive_action, { TextFile } }, State ).

%write_movement_metro_message( State , CurrentTickOffset , Destination ) ->

	%CarId = getAttribute( State , car_name ),
  	%Type = getAttribute( State , type ),

	%LastPosition = getAttribute( State , car_position ),

%	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"metro\" />\n", [ CurrentTickOffset , CarId , LastPosition , CarId , Type ] ),
%	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"metro\" />\n", [  CurrentTickOffset , CarId , Destination , CarId , Type ] ),


%	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

%	LogPID = ?getAttr(log_pid),

%	class_Actor:send_actor_message( LogPID, { receive_action, { TextFile } }, State ).
