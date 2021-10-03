========================================================================
    Fortran Console Application : "RSPT_L" Project Overview
========================================================================

Date: Oct 03, 2021

RSPT_L: A realization of Large-order Reyleigh-Shrodinger Perturbation theory for vibrational states of Linear Molecules

Author: Xuanhao Chang

RSPT_L is a fast Fortran program based on Intel oneAPI base toolkit including MKL library.
It allows one to calculate the vibrational state of linaer molecules using large order Reyleigh-Shrodinger perturbation theory.

Three input files should be prepared before running:

(a) RSPT_L.ini Some key data: directories, input files, output level, FM accuracy (manissa);

(b) RSPT_Molecule(Model).inp Main general inpit: parameters, RSPT series setting, etc.;

(c) PARA_Molecule(Model).mol Molecular properties: NQ, Harmonic frequencies of molecule;

Besides, bin file from Zero-order, cubic, quartic normal-odering Hamiltonian, Coriolis term and L_z operators should be prepared from Wolfram Mathematica programm.

/////////////////////////////////////////////////////////////////////////////
