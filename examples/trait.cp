--> "Version 0.1: usage..."

type Editor = {
  onKey: String -> String;
  doCut: String;
  showHelp: String;
};

type Version = { version : String };

editor = trait [self : Editor & Version] implements Editor => {
  onKey = \key -> "Pressing " ++ key;
  doCut = self.onKey "C-x" ++ " for cutting text";
  showHelp = "Version " ++ self.version ++ ": usage...";
};

version = trait => { version = "0.1" };

(new editor , version).showHelp
