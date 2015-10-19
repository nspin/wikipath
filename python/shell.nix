let
  pkgs = import <nixpkgs> {};
  bs4 = pkgs.buildPythonPackage {
    name = "beautifulsoup4-4.4.0";
    src = pkgs.fetchurl {
      url = http://www.crummy.com/software/BeautifulSoup/bs4/download/4.4/beautifulsoup4-4.4.0.tar.gz;
      sha256 = "1xhp57nr7aapn55wpk7i2kcv2cdamcn1parrm6dqnhv9iyl1vngs";
    };
    meta = {
      license = pkgs.stdenv.lib.license.mit;
    };
  };
in

pkgs.stdenv.mkDerivation {
  name = "wikipaths-python";
  version = "0.1.0.0";
  src = ./.;
  buildInputs = [ pkgs.python bs4 ];
}
