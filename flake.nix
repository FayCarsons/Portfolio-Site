{
  description = "Portfolio site with blog parser";
  
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages.default = pkgs.stdenv.mkDerivation {
          name = "portfolio-site";
          src = ./.;
          
          buildInputs = with pkgs; [ 
            ghc 
            cabal-install 
            nodejs
            nodePackages.npm  # Explicitly add npm
          ];
          
          buildPhase = ''
            echo "Building blog parser..."
            cd blog-parser
            cabal update
            cabal configure
            cabal build
            cabal run
            cd ..
            
            echo "Building frontend..."
            cd frontend
            npm ci
            npm run build
          '';
          
          installPhase = ''
            cp -r frontend/dist $out
          '';
        };
        
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [ 
            ghc 
            cabal-install 
            nodejs 
            nodePackages.npm 
          ];
        };
      });
}