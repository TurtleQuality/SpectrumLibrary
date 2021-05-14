# SpectrumLibrary
Handy routines for Spectrum code. But very messy

Ok so I was writing stuff back in the day on the Spectrum, one published game (Mazeball) and I contributed the compress/decompress code to the Multiface. Other than that, wrote some simple noisy games for myself that are now all lost, all using compiled basic and some very short assembly routines. Got back into programming on the Spectrum in 2018, and gathered this collection. Much is taken from L Break Into Program, or from World Of Spectrum or Spectrum Computing forums. Some of my own work here is misguided and will be taken out in future versions, like the self modifying bit cmd routine when a far simpler method was available.

I'm using Zeus as it's a handy all in one editor, assembler, monitor/emulator. The first bits setup a repeat function and a DIX(label) function, which most certainly won't work elsewhere. Malign 256 (code) Maligned will either keep code within a 256 byte boundary, or moan that the monkey is misbehaving if (code) exceeds 256 bytes.

Anyway I have a main source file which will appear when I get a game working, then 4 includes, a Spectrum Rom Disassembly, Spectrum System variables, my data labels, and this library file for code that could be used for multiple projects. So this is an incomplete working file. 

The BCD stuff came from https://www.chibiakumas.com/z80/advanced.php , I plan to test and modify it for my own purposes, score printing, hi score table etc... 
