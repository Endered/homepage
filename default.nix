{pkgs ? import <nixpkgs> {}}:
pkgs.mkShell
  {
    nativeBuildInputs = with pkgs; [ gauche lua nodejs ];
  }
