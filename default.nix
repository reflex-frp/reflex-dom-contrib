{ rpRef ? "ae415044d5ef35d947f0689a66f349ae9e2b7aee"
, rpSha ? "0mpipci4mxpaqfn2iqc6gwh0n11rvp1l2fz7g703gp12djr8srfd"
}:

let
  rp = builtins.fetchTarball {
    url = "https://github.com/reflex-frp/reflex-platform/archive/${rpRef}.tar.gz";
    sha256 = rpSha;
};

in
  (import rp {}).project ({ pkgs, ... }:
  let gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
        owner = "siers";
        repo = "nix-gitignore";
        rev = "4f2d85f2f1aa4c6bff2d9fcfd3caad443f35476e";
        sha256 = "1vzfi3i3fpl8wqs1yq95jzdi6cpaby80n8xwnwa8h2jvcw3j7kdz";
      }) {};
      doJailbreak = pkgs.haskell.lib.doJailbreak;
  in
  {
    name = "reflex-dom-contrib";
    overrides = self: super: with pkgs.haskell.lib;
      {
        reflex-dom-contrib = doJailbreak (super.reflex-dom-contrib); 
      };
    packages = {
      reflex-dom-contrib = (gitignore.gitignoreSource [] ./.);
    };
    shellToolOverrides = ghc: super: {
      ghcid = pkgs.haskellPackages.ghcid;
    };
    shells = {
      ghc = ["reflex-dom-contrib"];
      ghcjs = ["reflex-dom-contrib"];
    };

  })
