let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/cc54fb41d13736e92229c21627ea4f22199fee6b.tar.gz") {};
  ghcVersion = "ghc946";  # Corresponds to GHC 9.4.6
  ghcWithPackages = pkgs.haskell.packages.${ghcVersion}.ghcWithPackages (pkgs: with pkgs; [
    haskell-language-server
    # Add other Haskell dependencies here if needed
  ]);
in
pkgs.mkShell {
  buildInputs = [
    ghcWithPackages
    pkgs.stack
    pkgs.curlFull.dev
    pkgs.oha
    pkgs.zlib.dev
  ];

  shellHook = ''
    echo "Nix shell with GHC ${ghcVersion} and Haskell Language Server"
  '';
}
