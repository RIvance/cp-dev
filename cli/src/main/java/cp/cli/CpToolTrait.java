package cp.cli;

import java.io.File;

public interface CpToolTrait {
    void run(File file);
    void iterate(String code);
}
