# DMN

Public code repo for Default Mode Network (DMN) simulation implemented with Haskell Yampa.

## Prerequisite

### Simulation

- stack >= 1.9.3 (see [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/))

## Build & Run

### Simulation

```
path_to_project > stack init
path_to_project > stack build
path_to_project > stack exec DMN-exe
```

The results will be saved in `./result/`.

## Code Layout

All source codes are placed in the directory `./src/DMN`.

- Subdirectory `Utils` contains 3 modules: 
  - `DMN.Utils.RK`: the implementation of Runge-Kutta 4-4
  - `DMN.Utils.Constants`: contains all constants in the DMN
  - `DMN.Utils.Initial`: contains initial firing vector and initial value vector
- Subdirectory `ODEs` contains 2 modules:
  - `DMN.ODEs.Equations`: the implementation of all ODEs equations (generic version)
  - `DMN.ODEs.System`: the build-up of the entire ODE system based on `DMN.ODEs.Equations`
- Subdirectory `Control` contains 3 modules:
  - `DMN.Control.AuxSF`: the implementation of two auxiliary signal function:
    -  `oneStepCalc`: one step Runge-Kutta calculation, taking the result from the previous time point and return the result at the next time point based on Runge-Kutta 4-4.
    - `updateResFire`: update the firing vector and result vector at the new time point according to the previous firing vector and the primary result returned by the `oneStepCalc`
  - `DMN.Control.MainSF`: main signal function controlling the data flow. Implementations are based on Haskell FRP framework `Yampa`.
  - `DMN.Control.MainLoop`: main loop with side effects, providing RNG and saving simulation results into `.txt` file