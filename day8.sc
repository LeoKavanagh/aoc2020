case class Cmd(name: String, value: Int, line: Int)
case class Accs(acc: Int, currentLine: Int, history: List[Int])


def parseData(rawData: String): List[Cmd] = {
  rawData
    .split('\n')
    .zipWithIndex
    .map(a => (a._1.split(' '), a._2))
    .map(a => Cmd(a._1(0), a._1(1).toInt, a._2))
    .toList
}

def alreadyRun(cmd: Cmd, accs: Accs) = {
  accs.history.contains(cmd.line)
}

def updateAccs(cmd: Cmd, accs: Accs) = cmd match {
  case Cmd("nop", _, _) => Accs(accs.acc, accs.currentLine + 1, cmd.line :: accs.history)
  case Cmd("jmp", _, _) => Accs(accs.acc, accs.currentLine + cmd.value, cmd.line :: accs.history)
  case Cmd("acc", _, _) => Accs(accs.acc + cmd.value, accs.currentLine + 1, cmd.line :: accs.history)
}

def processCmds(cmds: List[Cmd], accs: Accs): Accs = {
  val nextCmd: Cmd = cmds(accs.currentLine)
  if (alreadyRun(nextCmd, accs)) accs
  else processCmds(cmds, updateAccs(nextCmd, accs))
}

def processCmds2(cmds: List[Cmd], accs: Accs): Accs = {
  if (accs.currentLine >= cmds.length) accs
  else if (alreadyRun(cmds(accs.currentLine), accs)) Accs(-1, -1, List())
  else processCmds2(cmds, updateAccs(cmds(accs.currentLine), accs))
}

def switchCommand(cmd: Cmd): Cmd = cmd match {
    case Cmd("nop", a, b) => Cmd("jmp", a, b)
    case Cmd("jmp", a, b) => Cmd("nop", b, b)
    case Cmd("acc", a, b) => Cmd("acc", a, b)
}

def makeNewCommands(cmds: List[Cmd], i: Int): List[Cmd] = {
  val tempData = cmds.toArray
  tempData(i) = switchCommand(tempData(i))
  tempData.toList
}

val realData = parseData(os.read(os.Path("/home/leo/repos/aoc2020/data/day8.csv")))

val ex = """nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"""
val parsedEx = parseData(ex)

// Example answer for Part 1
assert(processCmds(parsedEx, Accs(0, 0, List())).acc == 5)

// Actual answer to Part 1
val ans1: Accs = processCmds(realData, Accs(0, 0, List()))
println(s"Ans 1: ${ans1.acc}")

// Example answer to Part 2
assert((0 until parsedEx.length)
  .map(i => processCmds2(makeNewCommands(parsedEx, i), Accs(0, 0, List())))
  .filter(_.acc > -1)(0).acc == 8)

// Actual answer to Part 2
val ans2 = (0 until realData.length)
  .map(i => processCmds2(makeNewCommands(realData, i), Accs(0, 0, List())))
  .filter(_.acc > -1)(0)

println(s"Ans 2: ${ans2.acc}")
