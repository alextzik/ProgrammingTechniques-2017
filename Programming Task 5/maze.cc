#include "maze.hh"
#include <assert.h> 

/////////////////////
// Private helpers //
/////////////////////

// Take 2D expanded coordinates and compute the corresponding 1D
// array index
int Maze::getArrayIndex(const Location loc) const {


	int expandedNumber = 2 * numCols + 1;

	int cel = (loc.row *expandedNumber) + loc.col;
	return cel;

}

// Returns the expanded coordinates of the specified cell
// coordinates
Location Maze::getCellArrayCoord(int cellRow, int cellCol) const {


	int expandedRow;
	int expandedCol;
	expandedRow = 2 * cellRow + 1;
	expandedCol = 2 * cellCol + 1;
	Location loc (expandedRow, expandedCol);
	return loc;
}

// Returns the expanded coordinates of the wall on a specific side of
// a cell given in cell coordinates
Location Maze::getWallArrayCoord(int cellRow, int cellCol,
	Direction direction) const {


	int wallRow;
	int wallCol;

	Location loc = getCellArrayCoord(cellRow, cellCol);

	switch (direction)
	{

	case Direction::NORTH:
		wallRow = loc.row - 1;
		wallCol = loc.col;
		break;
	case Direction::EAST:
		wallRow = loc.row;
		wallCol = loc.col + 1;
		break;
	case Direction::SOUTH:
		wallRow = loc.row + 1;
		wallCol = loc.col;
		break;
	case Direction::WEST:
		wallRow = loc.row;
		wallCol = loc.col - 1;
		break;
	}

	Location helper(wallRow, wallCol);
	return helper;
}

//////////////////////////////////////////
// Constructors - Deconstructors - Copy //
//////////////////////////////////////////

// Initialize a new maze of size rows x cols
Maze::Maze(int rows, int cols) {

	numRows = rows;
	numCols = cols;

	Location loc = getCellArrayCoord(rows,cols);
	int cellsNumber = getArrayIndex(loc);
	
	cells = new MazeCell[cellsNumber];

	for (int i = 0; i < cellsNumber; i++)
	{
		cells[i] = MazeCell::EMPTY;
	}

}


Maze::Maze(const Maze &m) {

	numCols = m.getNumCols();
	numRows = m.getNumRows();
	start = m.getStart();
	end = m.getEnd();

	Location loc = getCellArrayCoord(numRows, numCols);
	int cellsNumber = getArrayIndex(loc);

	cells = new MazeCell[cellsNumber];
	clear();

	for (int i = 0; i < numRows; i++)
	{
		for (int j = 0; j < numCols; j++)
		{
			setCell(i, j, m.getCell(i, j));
			if (m.hasWall(i, j, Direction::NORTH))
			{
				setWall(i, j, Direction::NORTH);
			}

			if (m.hasWall(i, j, Direction::EAST))
			{
				setWall(i, j, Direction::EAST);
			}
			if (m.hasWall(i, j, Direction::WEST))
			{
				setWall(i, j, Direction::WEST);
			}

			if (m.hasWall(i, j, Direction::SOUTH))
			{
				setWall(i, j, Direction::SOUTH);
			}
		}
	}
}

Maze::~Maze() {
	delete[] cells;
}

///////////////////////
// Operator overload //
///////////////////////

// Maze assignment operator
Maze& Maze::operator=(const Maze &m) {

	if (this == &m)
		return *this;

	numCols = m.getNumCols();
	numRows = m.getNumRows();
	start = m.getStart();
	end = m.getEnd();

	Location loc = getCellArrayCoord(numRows, numCols);
	int cellsNumber = getArrayIndex(loc);

	cells = new MazeCell[cellsNumber];
	clear();

	for (int i = 0; i < numRows; i++)
	{
		for (int j = 0; j < numCols; j++)
		{
			setCell(i, j, m.getCell(i, j));
			if (m.hasWall(i, j, Direction::NORTH))
			{
				setWall(i, j, Direction::NORTH);
			}

			if (m.hasWall(i, j, Direction::EAST))
			{
				setWall(i, j, Direction::EAST);
			}
			if (m.hasWall(i, j, Direction::WEST))
			{
				setWall(i, j, Direction::WEST);
			}

			if (m.hasWall(i, j, Direction::SOUTH))
			{
				setWall(i, j, Direction::SOUTH);
			}
		}
	}
	return *this;
}

///////////////
// Accessors //
///////////////

// Returns the number of rows in the maze
int Maze::getNumRows() const {

	return numRows;
}

// Returns the number of columns in the maze
int Maze::getNumCols() const {

	return numCols;
}

// Returns the starting point in the maze
Location Maze::getStart() const {


	return start;
}

Location Maze::getEnd() const {


	return end;
}

// Returns the value of the specified
MazeCell Maze::getCell(int cellRow, int cellCol) const {

	//assert(cellRow<0 || cellRow >numRows - 1);
	//assert(cellCol < 0 || cellCol>numCols - 1);

	Location helper= getCellArrayCoord(cellRow, cellCol); // = getCellArrayCoord(cellRow, cellCol);
	

	int position = getArrayIndex(helper);

	return cells[position];
}

// Returns the cell-coordinates of the neighboring cell in the specified
// direction.  Trips an assertion if the given cell has no neighbor in the
// specified direction (e.g. the NORTH neighbor of cell (0,5)).
Location Maze::getNeighborCell(int cellRow, int cellCol, Direction direction) const {

	if (cellRow == 0)
	{
		assert(!(direction==Direction::NORTH));
	}
	
	if (cellRow == numRows - 1)
	{
		assert(!(direction==Direction::SOUTH));
	}

	if (cellCol == numCols - 1)
	{
		assert(!(direction == Direction::EAST));
	}

	if (cellCol == 0)
	{
		assert(!(direction == Direction::WEST));
	}

	int wallRow=cellRow;
	int wallCol=cellCol;

	
	switch (direction)
	{

	case Direction::NORTH:
		wallRow -= 1;
		break;
	case Direction::EAST:
		wallCol +=1;
		break;
	case Direction::SOUTH:
		wallRow += 1;
		break;
	case Direction::WEST:
		wallCol -= 1;
		break;
	}

	Location neighboor(wallRow, wallCol);
	return neighboor;

}


// Returns true if there is a wall in the specified direction from the
// given cell, false otherwise
bool Maze::hasWall(int cellRow, int cellCol, Direction direction) const {

	//assert(cellRow<0 || cellRow >numRows - 1);
	//assert(cellCol < 0 || cellCol>numCols - 1);

	Location testForWall = getWallArrayCoord(cellRow, cellCol, direction);

	int position = getArrayIndex(testForWall);

	if (cells[position] == MazeCell::WALL) return true;
	else return false;

}

// Returns true if the specified maze cell has been visited.
bool Maze::isVisited(int cellRow, int cellCol) const {

	//assert(cellRow<0 || cellRow >numRows - 1);
	//assert(cellCol < 0 || cellCol>numCols - 1);

	Location helper = getCellArrayCoord(cellRow, cellCol); 

	int position = getArrayIndex(helper);

	if (cells[position] == MazeCell::VISITED) return true;
	else return false;
}

////////////////
// Operations //
////////////////

void Maze::setStart(int row, int col) {

	start.row = row;
	start.col = col;
}

void Maze::setEnd(int row, int col) {

	end.row = row;
	end.col = col;
}

// Sets all cells and walls to be empty, so that the maze is
// completely cleared
void Maze::clear() {

	int expandedRowDim = 2 * numRows + 1;
	int expandedColDim = 2 * numCols + 1;
	int cellsNumber = expandedRowDim*expandedColDim;


	for (int i = 0; i < cellsNumber; i++)
	{
		cells[i] = MazeCell::EMPTY;
	}

}

// Places a wall at every location that can be a wall in the maze
void Maze::setAllWalls() {

	Location loc = getCellArrayCoord(numRows, numCols);
	int cellsNumber = getArrayIndex(loc);

	for (int i = 1; i <cellsNumber ; i=i+2)
	{
		cells[i] = MazeCell::WALL;
	}
}

void Maze::setCell(int cellRow, int cellCol, MazeCell val) {

	//assert(cellRow<0 || cellRow >numRows - 1);
	//assert(cellCol < 0 || cellCol>numCols - 1);

	Location helper= getCellArrayCoord(cellRow, cellCol);

	int position = getArrayIndex(helper);

	cells[position] = val;
}

// Puts a wall on the specified side of the given cell
void Maze::setWall(int cellRow, int cellCol, Direction direction) {

	//assert(cellRow<0 || cellRow >numRows - 1);
	//assert(cellCol < 0 || cellCol>numCols - 1);

	Location loc = getWallArrayCoord(cellRow, cellCol, direction);

	int position = getArrayIndex(loc);

	cells[position] = MazeCell::WALL;


}

// Removes a wall on the specified side of the given cell
void Maze::clearWall(int cellRow, int cellCol, Direction direction) {

	//assert(cellRow<0 || cellRow >numRows - 1);
	//assert(cellCol < 0 || cellCol>numCols - 1);

	Location loc = getWallArrayCoord(cellRow, cellCol, direction);
	int position = getArrayIndex(loc);

	cells[position] = MazeCell::EMPTY;
}

void Maze::setVisited(int cellRow, int cellCol) {

	
	Location helper = getCellArrayCoord(cellRow, cellCol);

	int position = getArrayIndex(helper);
	cells[position] = MazeCell::VISITED;

}
void Maze::print(ostream &os) const {
    
    os << numRows << " " << numCols << endl;
    int expRow = 2 * numRows + 1;
    int expCol = 2 * numCols + 1;
    int currentrow = 0;
    int currentcol = 0;
    for (int i = 0; i < expRow; i++)
    {
        for (int j = 0; j < expCol; j++)
        {
            if (i % 2 == 0)
            {
                Location loc(i,j);
                int position = getArrayIndex(loc);
                if (j % 2 == 0)
                {
                    os << "+";
                    if (j == (expCol - 1))
                    {
                        //currentcol = 0;
                        os << endl;
                    }
                }
                if (j % 2 == 1)
                {
                    if (cells[position] == MazeCell::WALL)
                    {
                        os << "---";
                    }
                    if (cells[position] == MazeCell::EMPTY)
                    {
                        os << "   ";
                    }
                }
            }
            if (i % 2 == 1)
            {
                Location loc(i, j);
                int position = getArrayIndex(loc);
                if (j%2==0)
                {
                    if (cells[position] == MazeCell::WALL)
                    {
                        os << "|";
                    }
                    
                    if (cells[position] == MazeCell::EMPTY)
                    {
                        os << " ";
                    }
                    if (j == (expCol - 1))
                    {
                        currentrow++;
                        currentcol = 0;
                        os << endl;
                    }
                }
                if (j % 2 == 1)
                {
                    if (currentrow == start.row && currentcol==start.col)
                    {
                        os <<" S ";
                        currentcol++;
                        continue;
                    }
                    if (currentrow == end.row && currentcol==end.col)
                    {
                        os << " E ";
                    }
                    else
                    {
                        os << "   ";
                    }
                    currentcol++;
                }
            }
        }
    }
    system("PAUSE");
}
