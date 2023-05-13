========================================================================
    Fortran Console Application : "RSPT_L" Project Overview
========================================================================

Date: Oct 03, 2021

RSPT_L: Large-order Reyleigh-Shrodinger (Degenereate) Perturbation theory for vibrational states of Linear Molecules

Author: Xuanhao Chang

RSPT_L is a Fortran program based on the Intel oneAPI base-toolkit with MKL library included in the HPC toolkit.
It allows to calculate the perturbation series of vibrational states of linaer molecules using large order Reyleigh-Shrodinger perturbation theory in the degenerate case, 
taking 2D-isotropic harmonic oscillator as zero-order approximation.
Depending on the perturbation term, the obtained series can be slowly convergent or quickly divergent, 
in this case an additional resummation procedure should be applied for obtaining the right sum of series. 
This program is slightly different from the RSPT_L_FM version, 
the RSPT_L allows to calculate model case, such as 2D Harmonic Oscillator with a simple coupling. 
but the Float-Multiprecision package, applied specifically to obtaining correct series, especially for the high order case, is not included in this program.

The comprehensive description of Degenerate-RSP theory can be found in our work:
(2023) Spectrochimica Acta Part A, 288, 122071. https://doi.org/10.1016/j.saa.2022.122071

The program can be compiled straighforwardly at the Window/Linux platform using the lasted version of FORTRAN compiler integrated in the intel oneAPI base-toolkit.


Three input files should be prepared before running:

(a) RSPT_L.ini Some key data: directories, input files, output level, FM accuracy (manissa);

(b) RSPT_Molecule(Model).inp Main general inpit: parameters, RSPT series setting, etc.;

(c) PARA_Molecule(Model).mol Molecular properties: NQ, Harmonic frequencies of molecule;

Bin file from Zero-order, cubic, quartic normal-odering Hamiltonian, Coriolis term and L_z operators should be prepared from Wolfram Mathematica program.

/////////////////////////////////////////////////////////////////////////////
