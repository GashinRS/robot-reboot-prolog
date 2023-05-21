# robot-reboot-prolog

This is a university project made @Ghent-University.

For this project I made a prolog implementation of the online game [Robot Reboot](https://www.robotreboot.com/challenge). It's a game played through the command line and it also includes the ability to generate random game boards and solve boards. 

## Rules 
Robot Reboot is based on the board game [Ricochet Robots](https://en.wikipedia.org/wiki/Ricochet_Robots). The goal of the game is to reach the bullseye with a specific robot, in our case the robot with index 0, with the least amount of plays possible. Note that in order to win, the robot with index 0 has to land on the exact coordinates of the bullseye, moving through the bullseye does not count. Robots will always move in a straight line until they either hit a wall or another robot. You can move the robots in whatever order you want.

## Playing the game
We can start a game with the following command:
```bash
swipl src/main.pl -- '--game={path}'
```
Where path has to be replaced with the path to a board. Two boards are included in the root of this directory. To move a robot, type out the index of the robot followed by the direction (U, D, R, L). The direction is not case sensitive. The following table includes all possible robots and their index:
| Robot index | Karakter  | Unicode |
| ----------- | --------- | ------- |
| 0           | ▣         | U+25A3  |
| 1           | ■         | U+25A0  |
| 2           | ▲         | U+250F  |
| 3           | ◆         | U+25C6  |
| 4           | ◇         | U+25C7  |
| 5           | ◈         | U+25C8  |
| 6           | ◉         | U+25C9  |
| 7           | ◩         | U+25E9  |
| 8           | ◭         | U+25ED  |
| 9           | ◲         | U+25F2  |

## Solving and generating boards
A given board can be solved by using the following command:
```bash
cat input | swipl src/main.pl -- '--solve'
```
The solve algorithm uses BFS and will always generate the optimal (shortest) answer.

Generating a board is done by using:
```bash
swipl src/main -- '--gen=[{amount of robots},{length},{height}]'
```
This will print out the generated board, as well as the optimal solution using the previously mentioned algorithm. Note that a board can only have between 1 and 10 robots (inclusive) due to the amount of robot characters we have.

## Testing
There are some very basic movement tests that you can run by using the following command:
```bash
swipl run_tests -t halt tests/test.pl  
```
There is also a test mode that can be used to test a single play. This can be done by using:
```bash
cat input | swipl src/main.pl -- '--test=[{Robot index},{Direction}]'
```
