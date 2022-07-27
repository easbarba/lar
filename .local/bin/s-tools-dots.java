// #!/usr/bin/java --source 17

import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.Files;
import java.io.IOException;

public class Dots {
    public static void main(String[] args) {
        Cli cli = new Cli(args);
        cli.runOption();
    }
}

class Actions {
    public static void pretend(Path sDir) {
        try {
            Files.walk(sDir, 999)
                .forEach((n) -> {
                        System.out.print("PRETENDING: ");
                        System.out.println(n.toString());});
        }
        catch (IOException e){
            System.out.println(e);
        }
    }

    public static void force(Path sDir) {
        try {
            Files.walk(sDir, 999)
                .forEach((n) -> {
                        System.out.print("FORCING: ");
                        System.out.println(n.toString());});
        }
        catch (IOException e){
            System.out.println(e);
        }
    }

    public static void deploy(Path sDir) {
        try {
            Files.walk(sDir, 999)
                .forEach((n) -> {
                        System.out.print("DEPLOYING: ");
                        System.out.println(n.toString());});
        }
        catch (IOException e){
            System.out.println(e);
        }
    }
}

class Cli {
    public String option;
    public Path folder;

    public Cli(String[] m) {
        this.option =  m[0];
        this.folder = getFolderPath(m[1]);
    }


    public Path getFolderPath(String fld) {
        return Paths.get(fld);
    }

    public String usage() {
        return """
            Usage: dots [options]
              -d, --deploy                     deploy dotfiles links
              -f, --force                      force redeployment of dotfiles links
              -p, --pretend                    mimic deployment of symbolic links
              -i, --info                       general information of internals commands""";
    }

    public void runOption() {
        switch(this.option) {
        case "-d":
            Actions.deploy(this.folder);
            break;
        case "-f":
            Actions.force(this.folder);
            break;
        case "-p":
            Actions.pretend(this.folder);
            break;
        default:
            System.out.println(usage());
        }
    }
}
