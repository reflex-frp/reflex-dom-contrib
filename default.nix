{ rpRef ? "4b4a2c8679d6f3042180d2974b31c43bfc506b91"
, rpSha ? "185i8ccqk0xc05ckd921fs7nnaj88gihmlazny1dk004hxq78cj2"
}:

let rp = builtins.fetchTarball {
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
  in
  {
    name = "reflex-dom-contrib";
    overrides = self: super: with pkgs.haskell.lib;
       {
       };
    packages = {
      reflex-dom-contrib = gitignore.gitignoreSource [] ./.;
    };
    shellToolOverrides = ghc: super: {
      ghcid = pkgs.haskellPackages.ghcid;
    };
    shells = {
      ghc = ["reflex-dom-contrib"];
      ghcjs = ["reflex-dom-contrib"];
    };

  })
