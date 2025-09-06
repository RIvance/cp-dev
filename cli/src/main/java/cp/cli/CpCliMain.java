package cp.cli;

import picocli.CommandLine;

import java.io.File;

@SuppressWarnings("unused")
@CommandLine.Command(
    name = "cp",
    description = "CP programming language CLI",
    subcommands = {
        CpCliMain.Run.class,
    }
)
public class CpCliMain implements Runnable {

    private final CpToolTrait core;
    public final CommandLine commandLine = new CommandLine(this, new InnerClassFactory(this));

    public CpCliMain(CpToolTrait core) {
        this.core = core;
        ReadEvalPrintLoop.initReporter();
    }

    @Override
    @SuppressWarnings("InfiniteLoopStatement")
    public void run() {
        try (var repl = new ReadEvalPrintLoop()) {
            while (true) {
                String source = repl.iterate();
                core.iterate(source);
            }
        }
    }

    @CommandLine.Command(
        name = "run",
        description = "Run a CP program"
    )
    class Run implements Runnable {

        @CommandLine.Parameters(
            index = "0",
            description = "The CP program file, with the '.cp' extension"
        )
        private File file;

        @Override
        public void run() { core.run(file); }
    }

}
