Game Design project for CS3110 Functional Programming @ Cornell University
=========
### Table of Contents

* [Team Members](#team-members)
* [Summary](#summary)
* [Instructions](#instructions)
* [Design and Implementation](#design-and-implementation)
* [Architecture](#architecture)
* [Code Design](#code-design)
* [Implementation](#implementation)
* [Testing](#testing)
* [Extensibility](#extensibility)
* [Known Problems](#known-problems)
* [Comments](#comments)

### <a name="team-members"></a>Team Members
* Sid Shekhar - https://github.com/sidshekhar/
* Philip Kinhasa - https://github.com/downtocode

### <a name="summary"></a> Summary:
In this design document, we will go over the instructions on how to run the game first. Then we will go over some of the design and implementation details especially regarding how we treated the state of the game with a different module that we created (apart from the modules given to us). Then we will talk about how we tested our implementation using print statements and especially using the GUI. Finally we will speak about how our program is extensible because of the modules we have in place and of some known problems encountered when using our version of the game.

### <a name="instructions"></a> Instructions:
To compile our game we just use the make Game command. Then we run the java so we can do make GUI. Then, after we press connect on the GUI, we run the two bots with the localhost and port number as arguments. We do this having four terminals open in the correct directory and typing in the following commands in each respectively (according to the above steps):./game.exe
java -jar gui_client.jar ./botname.exe localhost 10500 ./botname.exe localhost 10500

### <a name="design-and-implementation"></a> Design and Implementation:
Modules: We used most of the modules that were given to us such as Game, Util, Connection, Definitions, and Constants – from which we used different functions. The only provided module where we actually implemented the game was in Game where we updates handle_action and handle_time to deal with advancing the state of the game for every timestep in handle_time and changing the state of the game to reflect actions taken in handle_action. To store all the helper functions of handle_action and handle_time and to keep track of game data and team data, we created another module called State. Most importantly, we defined the type game in the Game module to equal the type game from the State module, which held mutable types for game data (game_d), directions, whether or not the players were invincible, and how much time had passed.

### <a name="architecture"></a> Architecture:
The State and Util interface allow Game to use functions that could aid in the implementation of the different updates and actions performed in the game. The base types for all the different types of objects used in the game such as bullets, player characters, team data are contained in the Definitions module. Additionally all the constants used throughout the game are held in the Constants module. The information from these two modules are accessible and are used by other modules in defining specific functions for the creation and movement of these objects.

### <a name="code-design"></a> Code Design: 
We used while loops to loop through the creation of different bullets with different velocities and angles and created records for each bullet created. We used records with mutable fields for the count – when keeping track of each bullet created – so that we could change some aspect of a bullet with every successive creation.

### <a name="implementation"></a> Implementation:
We used a top-down approach to implement our game – breaking it into its separate components before commencing the programming of separate functions and modules. The first challenge we ran into while coding was deciding how to store what state our game was in at any stage in the game. This led to our creation of a state module that kept track of state, team and game data, and also was a convenient place to store helpers to keep away from cluttering the Game module. We also encountered the challenge of thinking about how to store bullets in a structure where they could be accessed by id (such as a hashtable) but at the same time deliver an end result of a list of bullets to game data. Roughly we divided the labor by having one of us do handle_time and another do handle_action. A such each
of us added different functions that would be useful to us in State and at the same time utilized what we needed from Constants, Definitions and Util.

### <a name="testing"></a> Testing:
After every block of code either of us wrote that performed an action, we first checked if it compiled with the larger program and then of it was something that was to derive a result, we tested only that block separately on the command prompt and used print statements throughout the block to make sure that it works. In terms of the GUI, we tested not only whether the graphical updates were sent appropriately by seeing if they appeared in the GUI but we also kept in mind how the internal state of the game should be changing and so kept track of what an object once it appears on the GUI must do – in order to fulfill the properties of the game given to us.

### <a name="extensibility"></a> Extensibility:
One can easily add additional features to our code. One can add a new module in the Game, Shared, or Team folders depending on which part of the game one wants to change. If one wanted to add new bullet types, or new collectible items (besides) for example, one could add their type definition in the Definitions module and then could write functions to implement those types in either the Util module or the State module. Then these implementations could be initialized in handle_action and handle_time. More interesting bomb effects could be added by creating a new function in the state module describing what effect the bomb will have on the game data arguments.
To create neutral enemies that fire at both players, one could edit the team_data and game_data types to add an element “enemy” to the tuples. Then a type “enemy” can be defined in the same module detailing different elements that one wants the neutral enemy to have. This new type of enemy can then be updated through the State and Game modules as part of updating the central game data (changing the state of the game and the elements in it).

### <a name="known-problems"></a> Known Problems:
We know that our GUI sometimes lags when too many bullets are fired in succession and at certain stages of the game. There are also white clouds that form in the middle when clearing bullets – thus obstructing the view for us. Essentially, the GUI and the Game state are slightly different in that what we do to update the GUI does not fully reflect the changed game state underneath (where it works correctly).

### <a name="comments"></a> Comments:
We felt that this project was overall a fun experience to carry out because of the game-aspect and because we could see the results of our work in the GUI. We spent a good amount of time on the assignment about 3 days straight coding. We would have liked to see a working version of the GUI so that we could know what to work towards – this would have especially helped when we were testing.
