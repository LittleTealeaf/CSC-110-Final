/********
* Assembler.scala
* Author: <ENTER YOUR NAMES HERE>
* Date: Fall 2017, CSC110
*
* Implements a very simple interpreter for an assembler-like language
* called LC-q (a variant of LC-3)
*********/

import scala.io.StdIn._

// The Machine State
var program: List[List[String]] = List()          // The program itself
var programCounter: Int = 0                       // Where in the program are we?
// Include other machine state variables like memory, registers, conditionCode...

// Report an error and TERMINATE
def errorExit(msg: String) {
  println("ERROR: " + msg)
  sys.exit(0)  // This terminates the program
}

// Execute the print operation
def executePrint(line: List[String]) {
    // Get the register to println
    if (line.length != 2) errorExit("INVALID INSTRUCTION")
    val reg = line(1)(1) - '0' // Convert register Character to a number
    println("DEBUG: Printing out value of register " + reg)
 }

// Process the current line based on Program Counter
def processCurrentLine() {
  // Check out the line
  val line = program(programCounter)
  line.head match {
    case "PRINT" => executePrint(line)
    case _       => errorExit("INVALID INSTRUCTION") // Or not yet recognized
  }
  programCounter += 1 // Move on to the next line
}

// Read in the program
def readProgram(): List[List[String]] = {
  val line = readLine()   // Get current line
  if (line == null)
    // End of input reached
    List()
  else {
    // Read rest of Program and attach this line (parsed by spaces) to it
    val restOfProgram = readProgram()  // Read rest of List
    line.toUpperCase.split(" ").toList::restOfProgram
  }
}

// Print out the entire program (debugging purposes really)
def printProgram(program: List[List[String]]) {
  var lineNo = 0
  for (line <- program) {
    println(f"$lineNo:\t ${line.mkString("\t")}")
    lineNo += 1  // Increment the line number
  }
}

// Determine if the program is finished (programCounter is beyond end of program!)
def programFinished(): Boolean = false

// Main body (load and preprocess the program)
program = readProgram()

// Just a debug step (remove at some point!)
println("DEBUG: Here is the program")
println("==========================")
printProgram(program)
println("==========================")

// Identify all of the labels (useful to do this before hand for branching)
// ...

// Run the program starting at line 0
programCounter = 0
while (!programFinished()) {
  processCurrentLine()
}
