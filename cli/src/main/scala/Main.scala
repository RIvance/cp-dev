
import cp.tool.CpTool
import cp.cli.CpCliMain

@main
def main(args: String*): Unit = {
  val cli = new CpCliMain(CpTool)
  val exitCode = cli.commandLine.execute(args: _*)
  System.exit(exitCode)
}
