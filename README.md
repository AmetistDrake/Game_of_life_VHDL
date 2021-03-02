# Configurations for windows

## GHDL install
- get the latest binary version from the git repo (ghdl-0.37-mingw32-mcode.zip):\
 https://github.com/ghdl/ghdl/releases
- unzip it, then add GHDL bin folder to PATH variables
   - Search 'Edit the system environment variables' 
   - Go to: Advanced | Environment variables | User variables | Path | Edit | New
   - Add the the bin folder 


## GTKWave install 
- get the latest binary version from the source forge project\
  https://sourceforge.net/projects/gtkwave/files/
- add gtkwave folder to the PATH variables

# GHDL commands
(use them in the src folder from cmd)


## Compiling from cmd:
 - Create `work` and `output` directory in the `src` folder
 - Run in cmd:
 ```
 cmd /V /C "set entity=game_of_life_tb&& ghdl -a --ieee=synopsys --std=08 --workdir=work *.vhd && ghdl -m --ieee=synopsys --std=08 --workdir=work !entity! && ghdl -r --ieee=synopsys --std=08 --workdir=work !entity! --vcd="output/!entity!".vcd"
 ``` 

## GTKWave:
- Run in cmd:
```
gtkwave output/game_of_life_tb.vcd
``` 

_Note: GTKWave cannot output custom type signals (only bits) on the waveform._