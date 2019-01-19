# { rpRef ? "384cd850f3adf1d404bced2424b5f6efb0f415f2"
# , rpSha ? "1ws77prqx8khmp8j6br1ij4k2v4dlgv170r9fmg0p1jivfbn8y9d"
# }:
{ rpRef ? "f3ff81d519b226752c83eefd4df6718539c3efdc"
, rpSha ? "1ijxfwl36b9b2j4p9j3bv8vf7qfi570m1c5fjyvyac0gy0vi5g8j"
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
