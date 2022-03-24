/********
* Assembler.scala
* Author: Thomas Kwashnak (Solo Project)
* Date: Fall 2020, CSC110
*
* Implements a very simple interpreter for an assembler-like language
* called LC-q (a variant of LC-3)
*********/
import scala.io.StdIn._
import scala.io.Source

/*
* Instantiating the following variables:
*  - register: Quick-Acess Memory
*  - memory: Longer-Storage Memory
*  - condition: Result from storing or comparing values, is either -1, 0, or 1
*  - pos: Current line position of the program (starting at line 0)
*/
val register: Array[Int] = Array.ofDim[Int](8)
val memory: Array[Int] = Array.ofDim[Int](1024)
var condition: Int = 0
var pos: Int = 0

/*
* The program allows for different ways of inputting code:
* - Inputting the file name / path as the first parameter will attempt to open that file, reading all lines of code
* - If no parameters are listed, the code will instead accept direct user input, reading each line the user inputs until either a "null" or the phrase ";end" is entered.
* After the complete instructions is inputted, the program will first filter out lines that start with a ';' or empty lines, then split up each line into an array split by the spaces found. The first element in the array is the "function name" and the following elements are that function's parameters
*/
val program: List[Array[String]] = {
    if(args.length > 0) {
      //Reading the file from the source
      (for(line <- Source.fromFile(args(0)).getLines) yield line).toList
    } else {
      //Accepting user input until a null or ";end" is entered
      var ret: List[String] = List[String]()
      var in = "--"
      do {
        in = readLine()
        //Prevents any null lines to be included in the actual code
        if(in != null) ret = ret :+ in
      } while(in != null && in != ";end")
      //Returns the list
      ret
    }
}.filter((line: String) => {
  //Filters to only lines that have a length (not empty) and don't start with ; (indicating comments)
  line.length > 0 && line(0) != ';'
  //Uppercases each line, then splits the lines up into arrays by spaces
}).map((line: String) => line.toUpperCase.split(" "))

/*
* Returns the index position of the marked label
* Cycles through each line of the program, only checking the arguments of the lines titled "LABEL"
* For each line marked as a label, it checks if that line is the label that it needs, if so it returns that index.
* If there is no label, throws an error
*/
def gotoLabel(lbl:String): Int = {
  for(i <- program.indices if(program(i)(0) == "LABEL")) {
    //This code will only run for LABEL markers. If the label matches the paramter, will return that index
    if(program(i)(1) == lbl) return i
  }
  //Only runs if no matching label is found
  quitError("LABEL DOES NOT EXIST")
  -1 //To satisfy return type on a system quit
}

/*
* Sets the condition value to 1, 0, or -1 depending on the value passed through
* if value is negative, condition value will be -1
* if value is zero, condition value will be 0
* if value is positive, condition value will be 1
*/
def setCondition(value: Int) {
  //Dividing a number by it's absolute value results in the polarity of the number, since 0/0 would result in a math error, dividing it by the max of it's absolute value and 1, if the value is 0, it would result in 0/1 = 0
  condition = value / Math.max(1,value.abs)
}

/*
* Quits the program with the selected error message
*/
def quitError(msg: String) {
  println(s"ERROR: $msg.")
  sys.exit(0)
}

/*
* Parses a "Register" value from the string, returning the register index.
* Throws an error if it cannot be parsed
* Throws an error if the index is not within the range 0 and 7
*/
def parseRegister(reg:String): Int = {
  try {
    //attempts to parse the register number
    val n: Int = reg.drop(1).toInt
    //Checks if the number is out of the range 0 to 7
    if(n < 0 || n > 7) quitError(s"REGISTER OUT OF RANGE ($n)")
    //returns the number
    return n
  } catch {
    //If the code could not parse the number
    case _: Throwable => quitError(s"COULD NOT PARSE REGISTER: $reg")
  }
  //Only included to satisfy the return type, does not actually ever get run
  -1
}

/*
* Loads the value to the indicated register
* Condition value is set to 1, 0, or -1 based on the result
*/
def loadRegister(reg:Int, value:Int) {
    register(reg) = value
    setCondition(value)
}

/*
* Prints the value stored at the indicated register
*/
def printValue(reg:Int) {
    print(register(reg))
}

/*
* Defines the LDI function:
* Loads the integer value into the specified register.
* Condition value is set to 1, 0, or -1 based on the value
* Throws an error if the value could not be parsed into an integer
*/
def functLDI(pReg: String, pVal: String) {
  try {
    //Attempts to load the register, parsing the second value
    loadRegister(parseRegister(pReg),pVal.toInt)
  } catch {
    //Error when the pVal could not be parsed into an integer
    case e: Exception => quitError(e.toString)
  }
}

/*
* Defines the LDA function:
* Sets the first register parameter to the value stored in the memory address referenced by the second register paramater
* Condition value is set to 1, 0, or -1 based on the value
* Throws an error if the value stored in the second parameter is not within the range from 0 to 1023
*/
def functLDA(pRegA: String, pRegB: String) {
  //Gets the memory address stored in the second register
  val memInd: Int = register(parseRegister(pRegB))
  //Throws error if out of range
  if(memInd < 0 || memInd > 1023) quitError("MEMORY ADDRESS IS NOT IN RANGE")
  //Stores the value found in the memory, and sets the condition
  loadRegister(parseRegister(pRegA),memory(memInd))
}

/*
* Defines the STORE function:
* Stores the value in the first register parameter to the memory address referenced by the second register parameter
* Throws an error if the value stored in the second parameter is not within the range from 0 to 1023
*/
def functSTORE(pRegA: String, pRegB: String) {
  //Gets the memory address stored in the second register
  val memInd: Int = register(parseRegister(pRegB))
  //Throws an error if out of range
  if(memInd < 0 || memInd > 1023) quitError("MEMORY ADDRESS IS NOT IN RANGE")
  //Stores the value from the register into memory
  memory(memInd) = register(parseRegister(pRegA))
}

/*
* Defines the PRINT function:
* Prints the value stored in the register parameter
*/
def functPRINT(pReg: String) {
  println(register(parseRegister(pReg)))
}

/*
* Defines the ADD function:
* Adds the values of the second and third register, and stores it into the first register
* Condition value is set to 1, 0, or -1 based on the value
*/
def functADD(pRegA: String, pRegB: String, pRegC: String) {
  loadRegister(parseRegister(pRegA),register(parseRegister(pRegB)) + register(parseRegister(pRegC)))
}

/*
* Defines the MULT funciton:
* Multiples the values of the second and third register, and stores it into the first register
* Condition value is set to 1, 0, or -1 based on the value
*/
def functMULT(pRegA: String, pRegB: String, pRegC: String) {
  loadRegister(parseRegister(pRegA),register(parseRegister(pRegB)) * register(parseRegister(pRegC)))
}

/*
* Defines the AND funciton:
* Performs a bitwise AND function on the second and third register, and stores it into the first register
* Condition value is set to 1, 0, or -1 based on the value
*/
def functAND(pRegA: String, pRegB: String, pRegC: String) {
  loadRegister(parseRegister(pRegA),register(parseRegister(pRegB)) & register(parseRegister(pRegC)))
}

/*
* Defines the XOR function:
* Performs a bitwise XOR function on the second and third register, and stores it into the first register
* Condition value is set to 1, 0, or -1 based on the value
*/
def functXOR(pRegA: String, pRegB: String, pRegC: String) {
  loadRegister(parseRegister(pRegA),register(parseRegister(pRegB)) ^ register(parseRegister(pRegC)))
}

/*
* Cycles through each line until the position exceeds the program's length
* Uses a match case structure to identify the function based on the line's first parameters
*
* A few definitions for the following comments regarding each functions
* - "Parameter Register" means that the parameter is a register (R0, R1... R7)
* - "Parameter Register Value" is the value found in the register "Parameter Register"
* The above will have a prefix (first, second, third) infront of them, such that "first parameter register" would mean that the first parameter (after the function name) is a register name, and references that register position
*
* BRN, BRP, BRZ, and JMP commands:
*   These set the position manually to the position of the corresponding label. Because the program will still increase the position by 1 at the end of the cycle, the program will then start the next cycle on the line after the label it was just moved to
*/
while(pos < program.length) {
  try {
    //Matches the first value of the line, which is the function name
    program(pos)(0) match {
      //LDI: Stores the value of the second parameter into the first parameter register and updates the condition value
      case "LDI" => functLDI(program(pos)(1),program(pos)(2))
      //LDA: Stores the first parameter register value into the memory at the address of the second parameter register value
      case "LDA" => functLDA(program(pos)(1),program(pos)(2))
      //STORE: Stores the value in the first register parameter into the memory address stored in the value of the second register parameter
      case "STORE" => functSTORE(program(pos)(1),program(pos)(2))
      //PRINT: Prints the first parameter register value
      case "PRINT" => functPRINT(program(pos)(1))
      //ADD: Adds the second parameter register value and the third parameter register value. It stores this value into the first parameter register and updates the condition value
      case "ADD" => functADD(program(pos)(1),program(pos)(2),program(pos)(3))
      //MULT: Multiplies the second parameter register value and the third parameter register value. It stores this value into the first parameter register and updates the conditon code
      case "MULT" => functMULT(program(pos)(1),program(pos)(2),program(pos)(3))
      //AND: Computes the binary AND operation to the second parameter register value and the third parameter regsiter value. It stores this value into the first parameter regsiter and updates the condition code
      case "AND" => functAND(program(pos)(1),program(pos)(2),program(pos)(3))
      //XOR: Computes the binary XOR operation to the second parameter register value and the third parameter regsiter value. It stores this value into the first parameter regsiter and updates the condition code
      case "XOR" => functXOR(program(pos)(1),program(pos)(2),program(pos)(3))
      //LABEL: Does nothing (Nothing is needed, as the program identifies labels in the gotoLabel method)
      case "LABEL" => {}
      //BRN: If the condition value is negative, proceed to the label passed as the first parameter
      case "BRN" => if(condition == -1) {
        pos = gotoLabel(program(pos)(1))
      }
      //BRP: If the condition value is positive, proceed to the label passed as the first parameter
      case "BRP" => if(condition == 1) {
        pos = gotoLabel(program(pos)(1))
      }
      //BRZ: If the condition value is 0, proceed to the label passed as the first parameter
      case "BRZ" => if(condition == 0) {
        pos = gotoLabel(program(pos)(1))
      }
      //JMP: Proceed to the label passed as the first parameter
      case "JMP" => pos = gotoLabel(program(pos)(1))
      //ERROR: Command is not marked as above
      case _ => {
        quitError("COMMAND NOT FOUND")
      }
    }
  } catch {
    //Only occurs when the program does not have sufficient parameters for one of the functions. (Example: code reads "ADD R0 R1", since the ADD function requires 3 parameters)
    case _: Throwable => quitError("INSUFFICIENT PARAMETERS")
  }
  //Moves the program 1 line further into the program
  pos += 1
}
