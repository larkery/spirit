{stdenv, SDL2, gnumake, gcc, fetchFromGitHub}:
stdenv.mkDerivation {
  name = "whisper-main";
  src = fetchFromGitHub {
    owner =  "ggerganov";
    repo =  "whisper.cpp";
    rev =  "fa8dbdc8886150254748854387ea9240385800e2";
    sha256 =  "176MpooVQrq1dXC62h8Yyyhw6IjCA50tp1J4DQPSePQ=";
  };
  buildPhase = ''
    make main
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp main $out/bin
  '';
  buildInputs = [SDL2 gnumake gcc];
}
