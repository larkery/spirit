{config, pkgs, ...}:{
  systemd.services.spirit = {
    path = [pkgs.mimic];
    wantedBy = ["multi-user.target"];
    script = let whisper = pkgs.callPackage ./whisper.nix {} ; in
    ''
      # ick
      export WHISPER_PATH=${whisper}/bin/main
      export WHISPER_ARGS="[\"-m\" \"/data/ggml-base.en.bin\"]"
      ${pkgs.jre}/bin/java -jar ${./target/spirit.jar} ${./test.grammar} ${./spirit-config.edn}
    '';
  };
  
  services.nginx.virtualHosts."spirit.home" = {
    locations."/" = {
      proxyPass = "http://127.0.0.1:12321";
    };
  };
}
