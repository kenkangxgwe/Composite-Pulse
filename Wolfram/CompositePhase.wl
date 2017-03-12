(* ::Package:: *)

(*Insert half of the phases of a pulse set. Unit: \[Pi]*)
PulseSet[PhaseHalfList_]["PhaseHalfList"[]]           := PhaseHalfList;
(*Construct another half*)
PulseSet[PhaseHalfList_]["PulseList"[]]               := PulseHalfList~Join~Rest[Reverse[PulseHalfList]] \[Times] \[Pi];
(*Construct the dual set*)
PulseSet[PhaseHalfList_]["PulseNum"[]]                := Length[PulseSet[PhaseHalfList][PulseList]];
PulseSet[PhaseHalfList_]["DualPhaseHalfList"]         := Mod[PhaseHalfList + Table[(1/2) + (-1)^(n+1) \[Times] (1/2), {n, Length[PhaseHalfList]}], 2];
PulseSet[PhaseHalfList_]["DualSet"[]]                 := PulseSet[PulseSet[PhaseHalfList_]["DualPhaseHalfList"]];
PulseSet[PhaseHalfList_]["PulsePhase"[timestep_]]     := Piecewise[{PulseList, Table[\[Tau] - 1 <= timestep <= \[Tau] , {\[Tau] , PulseNum}]}\[Transpose]];
PulseSet[PhaseHalfList_]["PulseAmplitude"[timestep_]] := Sqrt[s/2] \[CapitalGamma] Sin[2 \[Pi] 0.5 timestep]^2 Boole[0 <= timestep <= PulseNum];
PulseSet[PhaseHalfList_]["RabiVector"[timestep_]]     := {PulseAmplitude[timestep] Cos[PulsePhaseCorrection[timestep]], PulseAmplitude[timestep] Sin[PulsePhaseCorrection[timestep]], zz};
PulseSet[PhaseHalfList_]["RabiVectorDisplayed"[timestep_]]   := 1/6 RabiVector[timestep];
PulseSet[PhaseHalfList_]["StateVector"[timestep_]]           := {x[timestep], y[timestep], z[timestep]};
PulseSet[PhaseHalfList_]["StateVectorTrajectory"[timestep_]] := Flatten[StateVector[timestep] /. StateVectorSolutions[[1]]];
PulseSet[PhaseHalfList_]["StateVectorTrajectory"[0]]         := -UnitVector[3, 3];

(*Declare of Constants*)
Constants["Gamma"] = 0.006 \[Times] 2 \[Pi] ;
Constants["Omega"] = 0;
Constants["zz"]    = 2 \[Pi] Constants["Omega"] ;
Constants["s"]     = 0.45 \[Times] 10^5;

(*Some Pulse Sets from the article*)
ThreePulses      = PulseSet[{0, 1} /2]
FivePulsesA      = PulseSet[{0, 5, 2} / 6];
SevenPulsesA     = PulseSet[{0, 11, 10, 17} / 12];
NinePulsesA      = PulseSet[{0, 0.635, 1.35, 0.553, 0.297}];
ThirteenPulsesA  = PulseSet[{0, 9, 42, 11, 8, 37, 2} / 24];
TwentyFivePulseA = PulseSet[{0, 5, 2, 5, 0, 11, 4, 1, 4, 11, 2, 7, 4} / 6]
