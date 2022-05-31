{ pkgs ? import <nixpkgs> {}
}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    R
    cacert
    rPackages.sdcMicro
    rPackages.readr
    rPackages.dplyr
    rPackages.httr
    rPackages.markdown
  ];

  shellHook = ''
    export LC_ALL=C.UTF-8
  '';
}
