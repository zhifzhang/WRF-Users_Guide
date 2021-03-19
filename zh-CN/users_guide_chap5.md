# 第五章 WRF模型

## 目录

1. [简介](#Introduction)

2. [安装WRF](#Installing_WRF)

3. [运行WRF](#Running_WRF)
	
	3.1 [ideal案例](#Idealized_Case)

	3.2 [real-data案例](#Real_Data_Case)
	
	3.3 [重启动运行](#Restart_Run)
	
	3.4 [双向反馈嵌套运行](#Two_Way_Nested)
	
	3.5 [使用ndown的单向嵌套运行](#One_Way_ndown)
	
	3.6 [移动嵌套运行](#Moving_Nested)
	
	3.7 [分析数据推动运行](#Analysis_Nudging)
	
	3.8 [观测数据推动运行](#Observation_Nudging)
	
	3.9 [全球范围运行](#Global_Run)
	
	3.10 [DFI运行](#DFI_Run)
	
	3.11 [使用sst_update选项](#SST_Update)
	
	3.12 [使用bucket_mm和bucket_J选项](#bucket_mm_and_J)
	
	3.13 [自适应时间步长](#Adaptive_Time_Stepping)
	
	3.14 [随机参数化方案](#Stochastic_Parameterization_Schemes)
	
	3.15 [Run-Time IO](#Run_Time_IO)
	
	3.16 [输出诊断](#Output_Diagnostics)
	
	3.17 [WRF-Hydro](#Hydro)
	
	3.18 [使用IO Quilting](#IO_Quilting)
	
	3.19 [使用物理套件](#Physics_Suites)
	
	3.20 [混合垂直坐标](#HVC)
	
	3.21 [使用多个横向条件文件](#Multiple_Lateral_Condition)

4. [各种应用的namelist示例](#Examples_namelists)

5. [检查输出文件](#Check_Output)

6. [故障排除](#Trouble_Shooting)

7. [物理与动力学选项](#Physics_Dynamics)

8. [PBL物理选项摘要](#PBL_Physics)

9. [微物理学选项摘要](#Microphysics)

10. [积云参数化选项摘要](#Cumulus_Parameterization)

11. [辐射物理选项摘要](#Radiation)

12. [namelist变量描述](#Namelist_Variables)

13. [WRF输出字段](#Output_Fields)

14. [特殊的WRF输出变量](#Special_Output)

<a id=Introduction></a>

## 简介

Advanced Research WRF（ARW）模型是完全可压缩的非静力模型（带有运行时的静水压选项）。它的垂直坐标可以选择为地形跟随（TF）或混合垂直坐标（HVC）静水压力坐标。网格交错采用Arakawa C-grid网格。模型在水平和垂直方向上均使用Runge-Kutta二阶和三阶时间积分方案，以及二阶至六阶对流方案。对声波和重力波模式使用了一个时间分割的小步长。动力学守恒的是标量变量。

WRF模型代码包含一个初始化程序（用于real-data、real.exe或ideal数据、ideal.exe，详见第4章），一个数值积分程序（wrf.exe），一个单独运行的对区域进行单向嵌套的程序（ndown.exe）和一个用于进行热带风暴模拟的程序（tc.exe）。WRF第4带版本支持多种功能，包括：

* real数据和ideal模拟

* 各种横向边界条件选项，用于real数据和ideal模拟

* 完整的物理选项和各种过滤器选项

* 正定对流方案

* 非静水压和静水压（运行时选项）

* 单向和双向反馈嵌套，以及移动嵌套

* 三维分析数据推动

* 观测数据推动

* 区域和全球应用

* 数字滤波器初始化

* 子区域中的垂直细化

### 其他参考资料

* [WRF教程演示](http://www.mmm.ucar.edu/wrf/users/supports/tutorial.html)

* [WRF-ARW技术说明](https://www2.mmm.ucar.edu/wrf/users/docs/technote/contents.html)

* 有关软件需求请参见本文档第2章。

<a id=Installing_WRF></a>

## 安装WRF

在编译WRF代码之前，请按照[How to Compile WRF](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/compilation_tutorial.php )页面上的`System Environment Tests`检查系统是否满足所有要求。

下一步是确保安装必要的库。netCDF库是构建WRF的唯一必需库，但根据需要的应用程序，可能需要其他库（例如，使用多个处理器运行的MPI库等）。如果尚未安装netCDF，请按照[How to Compile WRF](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/compilation_tutorial.php )页面的`Building Libraries`部分中的安装说明（以及其他说明）操作。否则，跳转到`Library Compatibility Tests`部分，以确保您的库与将用于构建WRF的编译器兼容。通过以下命令，确保正确设置netCDF库的环境变量路径（示例为csh）

```
setenv NETCDF path-to-netcdf-library/netcdf
setenv PATH path-to-netcdf-library/netcdf/bin
```

通常，netCDF库及其include/目录是并置的。如果不是这种情况，请手动创建目录，同时链接netCDF lib和include目录到此目录中，并使用环境变量设置该目录的路径。例如：

```
netcdf_links/lib -> /netcdf-lib-dir/lib 
netcdf_links/include -> /where-include-dir-is/include

setenv NETCDF /directory-where-netcdf_links-is/netcdf_links
```

如果在Linux计算机上使用了PGI、Intel或gfortran编译器，请确保使用相同的编译器安装netCDF。使用NETCDF环境变量指向PGI/Intel/gnu编译的netCDF库。

**提示**：如果使用netCDF-4，请确保在安装时未激活新功能（例如基于HDF5的并行I/O），除非您打算使用netCDF-4的压缩功能（更新信息详见下述）。

可以从[WRF网站](http://www2.mmm.ucar.edu/wrf/users/download/get_source.html )下载WRF的源代码。在WRF/目录中包含：

文件/目录名称|描述
-------------|----
Makefile     |顶级makefile文件
README       |WRF/ARW核心的一般信息
README.md    |重要链接和注册信息
Registry/    |WRF注册文件目录
arch/        |收集编译选项的目录
chem/        |WRF-chem模型，由NOAA/GSD提供支持
clean        |用于清除创建的文件和可执行文件的脚本
compile      |用于编译WRF代码的脚本
configure    |用于创建configure.wrf编译文件的脚本

doc/         |有关模型各种功能的信息
dyn_em/      |ARW动力学与数值目录
dyn_nmm/     |NMM动力学和数值目录，由DTC提供支持
external/    |包含外部程序包的目录，例如IO、计时和MPI的程序包
frame/       |包含WRF框架模块的目录
hydro/       |WRF-hydro，由[NCAR/RAL提供支持](https://ral.ucar.edu/projects/wrf_hydro/overview)
inc/         |包含“include”文件的目录
main/        |包含主要例程（如wrf.F）和编译后所有可执行文件的目录
phys/        |包含所有物理模块的目录
run/         |运行WRF的目录
share/       |包含主要WRF中间层和WRF I/O的模块的目录
test/        |包含测试案例的目录，可用于运行WRF
tools/       |包含开发人员工具的目录
var/         |WRF数据同化
wrftladj/    |WRFPLUS

编译和运行模型的步骤为：

1. 配置：生成配置文件用于编译

2. 编译：编译代码

3. 运行模型

转到WRF顶级目录并输入：

`./configure

WRF模型的构建允许在configure命令中使用一些选项。

`./configure –d`：在打开调试选项的情况下构建代码

`./configure –D`：与–d相同，但加上了边界和范围检查、未初始化的变量、浮动陷阱

`./configure –r8`：构建代码以使用64位实数进行计算和输出

对于任何`./configure`命令，都会显示计算机的选项列表。每个选项组合了一个操作系统、一个编译器类型和一个并行选项。由于配置脚本不会检查系统上实际安装了哪些编译器，因此请确保只在可用的选项中进行选择。并行选项包括：

1. 编译为单处理器作业（serial）

2. 使用OpenMP共享内存（smpar）

3. 为多处理器作业使用分布式内存并行化（dmpar）选项

4. 共享内存和分布式内存选项的组合（dm+sm）

做出选择后，将出现用于编译嵌套的第二个选择。例如，在Linux计算机上，上述步骤可能类似于：

```
> setenv NETCDF /usr/local/netcdf-pgi 
> ./configure

checking for perl5... no

checking for perl... found /usr/bin/perl (perl)

Will use NETCDF in dir: /glade/apps/opt/netcdf/4.3.0/intel/12.1.5

HDF5 not set in environment. Will configure WRF for use without.

PHDF5 not set in environment. Will configure WRF for use without.

Will use 'time' to report timing information

$JASPERLIB or $JASPERINC not found in environment, configuring to build without grib2 I/O...

-------------------------------------------------- ----------------------

Please select from among the following Linux x86_64 options:

  1. (serial)   2. (smpar)   3. (dmpar)   4. (dm+sm)   PGI (pgf90/gcc)
  5. (serial)   6. (smpar)   7. (dmpar)   8. (dm+sm)   PGI (pgf90/pgcc): SGI MPT 
  9. (serial)  10. (smpar)  11. (dmpar)  12. (dm+sm)   PGI (pgf90/gcc): PGI accelerator
 13. (serial)  14. (smpar)  15. (dmpar)  16. (dm+sm)   INTEL (ifort/icc)
                                         17. (dm+sm)   INTEL (ifort/icc): Xeon Phi (MIC architecture)
 18. (serial)  19. (smpar)  20. (dmpar)  21. (dm+sm)   INTEL (ifort/icc): Xeon (SNB with AVX mods)
 22. (serial)  23. (smpar)  24. (dmpar)  25. (dm+sm)   INTEL (ifort/icc): SGI MPT 
 26. (serial)  27. (smpar)  28. (dmpar)  29. (dm+sm)   INTEL (ifort/icc): IBM POE 
 30. (serial)               31. (dmpar)                PATHSCALE (pathf90/pathcc)
 32. (serial)  33. (smpar)  34. (dmpar)  35. (dm+sm)   GNU (gfortran/gcc)
 36. (serial)  37. (smpar)  38. (dmpar)  39. (dm+sm)   IBM (xlf90_r/cc_r)
 40. (serial)  41. (smpar)  42. (dmpar)  43. (dm+sm)   PGI (ftn/gcc): Cray XC CLE 
 44. (serial)  45. (smpar)  46. (dmpar)  47. (dm+sm)   CRAY CCE (ftn/cc): Cray XE and XC
 48. (serial)  49. (smpar)  50. (dmpar)  51. (dm+sm)   INTEL (ftn/icc): Cray XC
 52. (serial)  53. (smpar)  54. (dmpar)  55. (dm+sm)   PGI (pgf90/pgcc)
 56. (serial)  57. (smpar)  58. (dmpar)  59. (dm+sm)   PGI (pgf90/gcc): -f90=pgf90
 60. (serial)  61. (smpar)  62. (dmpar)  63. (dm+sm)   PGI (pgf90/pgcc): -f90=pgf90
 64. (serial)  65. (smpar)  66. (dmpar)  67. (dm+sm)   INTEL (ifort/icc): HSW/BDW
 68. (serial)  69. (smpar)  70. (dmpar)  71. (dm+sm)   INTEL (ifort/icc): KNL MIC 
 72. (serial)  73. (smpar)  74. (dmpar)  75. (dm+sm)   FUJITSU (frtpx/fccpx): FX10/FX100 SPARC64 IXfx/Xlfx

Enter selection [1-75] : ------------------------------------------------------------------------

Compile for nesting? (0=no nesting, 1=basic, 2=preset moves, 3=vortex following) [default 0]: 
```

输入最适合您的计算机和应用程序的适当选项。

当按下回车键时，将创建一个`configure.wrf`文件。如有必要，你可以在此文件中编辑编译选项/路径。

**提示**：从简单的事情开始（例如串行构建）是有帮助的。如果成功，则继续构建dmpar或smpar代码（除非您非常熟悉dm+sm，否则不建议使用它）。当您更改一个注册文件或在配置步骤中更改了选项时，请记住在每次编译之间输入`./clean –a`命令。

**提示**：如果您想使用由Argonne国家实验室开发的[并行netCDF（p-netCDF）](http://trac.mcs.anl.gov/projects/parallel-netcdf )，则需要单独安装p-netCDF，并使用环境变量PNETCDF设置路径：

`setenv PNETCDF path-to-pnetcdf-library

要编译代码，请键入:

`./compile

然后将出现以下选择：

```
Usage:

compile [-j n] wrf           compile wrf in run dir (Note, no real.exe, ndown.exe or ideal.exe generated)

or choose a test case (see README_test_cases for details):
 
	compile [-j n] em_b_wave
	compile [-j n] em_convrad 

	compile [-j n] em_esmf_exp (example only)
	compile [-j n] em_fire
	compile [-j n] em_grav2d_x

	compile [-j n] em_heldsuarez
	compile [-j n] em_hill2d_x

	compile [-j n] em_les
	compile [-j n] em_quarter_ss
	compile [-j n] em_real

	compile [-j n] em_seabreeze2d_x
	compile [-j n] em_squall2d_x
	compile [-j n] em_squall2d_y
	compile [-j n] em_tropical_cyclone

	compile [-j n] nmm_real (NMM solver)
	compile [-j n] nmm_tropical_cyclone (NMM solver)
 
compile -j n            parallel make using n tasks if supported (default 2)
compile –h              help message
```

其中cm代表ARW动态解算器（即“Eulerian mass-coordinate”解算器）。键入以上内容之一进行编译。如果你想切换到一个不同的测试案例，则需要重新编译新的案例。重新编译是创建新的初始化可执行文件（即real.exe和ideal.exe——对于每个ideal测试用案例需要不同的ideal.exe），而wrf.exe文件对于所有测试案例都是相同的。

如果要删除所有目标文件（除了那些在external/目录下的）和可执行文件，则输入`./clean`命令。

输入`./clean -a`命令以删除所有目录中的编译文件，包括configure.wrf（原始的configure.wrf将保存为configure.wrf.backup）。如果您已编辑configure.wrf或任何注册文件，则需要使用`./clean -a`命令。

如果检测到所有支持库都可用的前提下，则默认将使用netCDF4压缩函数进行编译。此选项通常会将文件大小减少50%以上，但请注意，输出可能需要更长的写入时间。如果所需的库不存在，将自动使用经典netCDF进行编译。也可以通过在编译前设置环境变量NETCDF_classic（setenv NETCDF_classic 1）来强制使用经典netCDF进行编译。

有关更多详细信息，请访问[此网站](http://www2.mmm.ucar.edu/wrf/users/building_netcdf4.html)

### Ideal案例

Ideal案例是一种用于模拟简单的测试范围广泛的空间和时间尺度的手段。测试案例再现已知的解决方案（解析的、收敛的等）。这些案例为其他Ideal实验提供了一个起点（例如，修改一个测试案例来衡量结果的差异）。

对于任何2D测试案例（在案例名称中有标记），必须使用串行或OpenMP（smpar）编译选项。此外，在配置时只能选择`0=no nesting`选项。对于其他所有案例，可以使用串行或并行（dmpar）和嵌套（除了`em_scm_xy`案例，它是一个1-D案例，必须串行编译，没有嵌套）。假设您要编译并运行二维案例，请键入：

`./compile em_squall2d_x >& compile.log

成功编译后，您应该在`main/`目录中创建两个可执行文件：`ideal.exe`和`wrf.exe`。这两个可执行文件将链接到相应的`test/case_name`和`run/`目录。`cd`到任一目录以运行模型。

将整个编译的标准错误和输出信息保存到文件是一个好的操作习惯（如上所示使用`>&`命令）。当不存在可执行文件时，此输出可用于帮助诊断编译错误。

### real-data案例

一个real-data案例使用的气象输入主要来源于以前的预测或分析，可能来自具有相对粗略分辨率的大尺度区域（例如全球）。一个real-data案例将提供一个三维预测或模拟。对于一个real-data案例，键入：

`./compile em_real >& compile.log

编译成功后，它将在`main/`目录中创建四个可执行文件：

real.exe：用于real-data案例的WRF初始化

ndown.exe：用于单向嵌套

wrf.exe：WRF模型

tc.exe：TC模型

这些可执行文件将链接到`test/em_real`和`run/`目录。`cd`到这两个目录之一以运行模型。

<a id=Running_WRF></a>

## 运行WRF

可以在`run/`目录或`test/case_name`目录中运行模型可执行文件。在这两种情况下，您都应该在目录下看到可执行文件、链接文件（主要用于real-data情况）和一个或多个namelist.input文件。

**提示**：如果您想在其他目录中运行模型可执行文件，请将`test/em_*`目录中的文件复制或链接到该目录，然后从那里运行。

<a id=Idealized_Case></a>

### ideal案例

假设测试案例`em_squall2d_x`已编译。要运行，请键入:

`cd test/em_squall2d_x

编辑namelist.input文件以更改积分长度、输出频率、区域大小、时间步长、物理选项和其他参数（详细信息请参阅`WRF/run/`目录下的README.namelist，或者[namelist变量描述](#Namelist_Variables)）。

如果在测试用例目录中看到一个名为`run_me_first.csh`的脚本，请通过键入以下内容首先运行该脚本：

`./run_me_first.csh

这会链接一些运行案例所需的物理数据文件。

要运行初始化程序，请键入:

`./ideal.exe >& ideal.out

该程序通常将读取位于案例目录中的输入探测文件，并生成初始条件文件`wrfinput_d01`。由于ideal案例不需要横向边界文件，因为边界条件通过namelist选项在代码中进行处理。如果工作成功，`ideal.out`文件的结尾应该是：

`wrf: SUCCESS COMPLETE IDEAL INIT

要运行模型，请键入

`./wrf.exe >& wrf.out

或者对于使用MPI（dmpar）选项编译的3D测试案例（注意，对于不同的机器和不同的MPI安装，MPI运行的执行命令可能不同），请键入

`mpirun –np 4 ./wrf.exe

如果成功，则将wrf输出文件写入到名为`wrfout_d01_0001-01-01_00:00:00.`的文件中。

成对的`rsl.out.*`和`rsl.error.*`文件将与MPI运行一起出现。这些是标准输出和错误文件。使用的每个处理器将有一对。

如果模型运行成功，则在`wrf.out`或`rsl.*.0000`文件结尾，打印的内容应为：`wrf: SUCCESS COMPLETE WRF`。

输出文件`wrfout_d01_0001-01-01*`和`wrfrst*`应该存在于运行目录中，具体取决于为输出指定namelist变量的方式。这些文件上的时间戳记开始于namelist文件中的开始时间。

<a id=Real_Data_Case></a>

### real-data案例

要运行real-data案例，请通过键入`cd`进入工作目录：

`cd test/em_real (or cd run)

编辑目录中的namelist.input模板文件，以匹配您的案例。

运行real-data案例需要先成功运行WRF预处理系统（WPS）。确保来自WPS的`met_em.*`文件在运行目录中可见（可采用链接或复制文件的方式）：

```
cd test/em_real
ln –s ../../..WPS/met_em* .
```

确保在namelist.input文件的`&time_control`和`&domains`部分中编辑变量，以匹配您的案例（详见[namelist变量描述](#Namelist_Variables)）：

```
&time_control
 run_days                            = 0,
 run_hours                           = 36,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = 2019, 2019, 2019,
 start_month                         = 09,   09,   09,
 start_day                           = 04,   04,   04,
 start_hour                          = 12,   12,   12,
 end_year                            = 2019, 2019, 2019,
 end_month                           = 09,   09,   09,
 end_day                             = 06,   06,   06,
 end_hour                            = 00,   00,   00,
 interval_seconds                    = 21600
 input_from_file                     = .true.,.true.,.true.,
 history_interval                    = 180,  60,   60,
 frames_per_outfile                  = 1000, 1000, 1000,
/
 
&domains
 time_step                           = 90,
 max_dom                             = 1,
 e_we                                = 150,    220,   200,
 e_sn                                = 130,    214,   210,
 e_vert                              = 45,     45,    45,
 p_top_requested                     = 5000,
 num_metgrid_levels                  = 34,
 num_metgrid_soil_levels             = 4,
 dx                                  = 15000,
 dy                                  = 15000,
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 1,     53,    30,
 j_parent_start                      = 1,     25,    30,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
/
```

确保区域的日期和维度与WPS中设置的一致。如果只使用一个区域，则只读取第一列中的条目，而忽略其他列。

在`&domains`中用于辅助垂直插值的其他选项包括：

```
interp_type                         = 2
extrap_type                         = 2
t_extrap_type                       = 2
lowest_lev_from_sfc                 = .false.
use_levels_below_ground             = .true.
use_surface                         = .true.
lagrange_order                      = 1
force_sfc_in_vinterp                = 1
zap_close_levels                    = 500
sfcp_to_sfcp                        = .false.
adjust_heights                      = .false.
smooth_cg_topo                      = .false.
```

real.exe程序是针对real-data案例的初始化程序。它获取WPS程序的二维输出（在`met_em*`文件中），对三维气象场和地下土壤数据执行垂直插值，并创建边界和初始条件文件以输入到wrf.exe程序。

要运行使用串行或OpenMP（smpar）选项进行编译的real-data初始化程序，请键入：

`./real.exe >& real.out

成功完成工作后，应在`real.out`文件的末尾打印`real_em: SUCCESS EM_REAL INIT`。它还应产生`wrfinput_d0*`（每个区域一个文件）和`wrfbdy_d01`文件，这个文件都是运行wrf.exe前必要的。

运行WRF：

`./wrf.exe >& wrf.out

成功运行后，应产生一个或几个名称为`wrfout_d<domain>_<date>`的输出文件，其中<domain>代表区域ID，<date>代表日期字符串，格式为yyyy-mm-dd_hh:mm:ss。例如，如果您在UTC时间2000年1月24日12：00启动模型，则第一个输出文件应具有以下名称：

`wrfout_d01_2000-01-24_12:00:00

文件名上的时间戳始终是第一次写入输出文件的时间。键入以下命令检查写入输出文件的时间：

`ncdump -v Times wrfout_d01_2000-01-24_12:00:00

您可能还有其他wrfout文件，具体取决于namelist选项（分割输出文件的频率由namelist选项`frames_per_outfile`来确定）。如果您在总积分时间内设置了重启动文件频率（在namelist.input中的`restart_interval`选项），则也可以创建重启动文件。重启动文件应具有以下命名：

`wrfrst_d<domain>_<date>

重启动文件上的时间戳记是该重新启动文件的时刻。

对于DM（分布式内存）并行系统，将需要某种形式的`mpirun`命令来运行可执行文件。例如，在Linux群集上，使用4个处理器运行MPI代码的命令可能类似于：

```
mpirun -np 4 ./real.exe
mpirun -np 4 ./wrf.exe
```

或者

`mpiexec_mpt ./wrf.exe`（在NCAR的cheyenne上）  






































<a id=Restart_Run></a>

### 重启动运行

重新启动运行使用户可以将运行延长到更长的仿真周期。它实际上是由几个较短的运行组成的连续运行。因此，一次或多次重新启动运行结束时的结果应与没有任何重新启动的单次运行相同。
为了进行重启运行，必须首先创建一个重启文件。这可以通过将名称列表变量restart_interval（默认单位为分钟）设置为等于或小于第一次模型运行中的模拟长度来完成，如run_ *变量或start_ *和end_ *次数所指定。当模型达到编写重新启动文件的时间时，将写入名为wrfrst_d <domain> _ <date>的重新启动文件。日期字符串表示重新启动文件有效的时间。
当开始重新启动运行时，编辑namelist.input文件，以便将start_ *时间设置为重新启动时间（即重新启动文件的写入时间）。必须设置的另一个名称列表变量是重新启动，此变量应设置为.true。重新运行。

总之，应修改以下名称列表条目：
start_*, end_*: 	start and end times for restart model integration
restart: 			logical to indicate whether the run is a restart or not
如果在重新启动运行中更改了历史记录和重新启动间隔，并且结果与预期不符，请使用名称列表‘override_restart_timers = .true.’.
如果在重新启动运行的时候需要历史导出记录，用‘write_hist_at_0h_rst = .true.’
提示：通常情况下，重新启动文件的大小是历史文件的几倍，因此可能会发现甚至是可以以netCDF格式（frame_per_outfile = 1）将单个模型历史记录输出时间写入文件，但是可能编写重启文件失败。这是因为基本的netCDF文件支持仅为2Gb（从WRF v3.9开始，默认情况下取消了此限制）。有两种解决方案。首先是简单地设置名称列表选项io_form_restart = 102（而不是2），这将强制将重新启动文件写入多个文件，每个处理器一个。只要使用相同数量的处理器重新启动模型，该选项就可以正常工作（无论如何，都应使用相同数量的处理器重新启动模型）。第二种解决方案是使用netCDF大文件支持选项重新编译代码（对于v3.9之前的WRF版本：请参阅本章中“安装WRF”部分）。

<a id=Two_Way_Nested></a>

### 双向反馈嵌套运行

双向嵌套运行是具有不同网格分辨率的多个域同时运行并彼此通信：较粗coarser的域为嵌套提供边界值，嵌套将其计算反馈给较粗的域。 该模型可以处理同一级别的嵌套（没有重叠的嵌套）和多个嵌套级别（伸缩）的多个域。
在准备嵌套运行时，请确保使用基本的嵌套选项（option1）编译代码。
启动嵌套运行的大多数选项都是通过名称列表处理的。namelist.input文件中具有多列条目的所有变量都需要谨慎编辑。从名称列表模板开始。以下是要修改的关键名称列表变量：
start _ *，end_ *：嵌套的开始和结束模拟次数
input_from_file：嵌套是否需要输入文件（例如wrfinput_d02）。这通常用于实际数据案例，因为嵌套输入文件包含嵌套地形和土地信息。
fine_input_stream：在嵌套初始化中使用嵌套输入文件中的哪些fields。要使用的字段在Registry.EM中定义。通常，它们包括静态场（例如地形和土地利用）和遮盖的表面场（例如体表温度，土壤湿度和温度）。对于在比粗域晚的时间开始的嵌套很有用。
max_dom：要运行的域总数。例如，如果要具有一个粗略域和一个嵌套，请将此变量设置为2。
grid_id：wrfout命名约定中使用的域标识符。最粗糙的网格的grid_id必须为1。
parent_id：用于指示嵌套的父域。使用grid_id值。
i_parent_start / j_parent_start：嵌套域在其父域中的左下角起始索引。这些参数应与namelist.wps中的相同。
parent_grid_ratio：父级到嵌套域的整数网格尺寸比。通常，在实际数据应用中使用奇数比率。
parent_time_step_ratio：嵌套域的整数时步比。它可能与parent_grid_ratio不同，尽管通常将它们设置为相同。
feedback：这是定义双向嵌套（或单向嵌套）运行的关键setup。启用反馈后，在嵌套点的重合点处，变量的值（质量点的单元格值的平均值和水平动量点的单元格面值的平均值）将覆盖粗糙域的值。对于屏蔽的字段，仅反馈并置点处的单点值。如果parent_grid_ratio是偶数，则将西南角点值的任意选择用于反馈。这就是最好在此选项中使用奇数parent_grid_ratio的原因。禁用反馈后，等效于单向嵌套运行，因为嵌套结果不会反映在父域中。
smooth_option：如果启用了反馈，则这是嵌套区域中父域的平滑选项。共有三个选项：0 =不平滑； 1 = 1-2-1平滑； 2 =平滑-去平滑。
3-D理想情况
对于3D理想情况，不需要嵌套输入文件。此处的关键是namelist.input文件的规范。该模型的作用是从粗糙域字段内插嵌套中所需的所有变量。组
input_from_file = T，F，

实际数据案例
对于实际数据情况，支持三个输入选项。
第一个类似于运行理想化案例。那就是从粗糙域（input_from_file = T，F）内插嵌套的所有字段。此选项的缺点很明显：一个不会从高分辨率的静态字段（例如地形，土地使用等）中受益。
第二个选项是为每个域设置input_from_file = T，这意味着该嵌套将有一个嵌套的wrfinput文件可读取。此选项的局限性在于，它仅允许该嵌套与粗略域同时启动。
第三个选项是，除了为每个域设置input_from_file = T，还要为每个域设置fine_input_stream = 2。为什么取值为2？这基于注册设置，该设置指定要从辅助输入流2中读取的某些字段。此选项允许嵌套初始化使用从粗域，静态字段和掩码以及时间插值的3-D气象字段-嵌套wrfinput的表面场变化；因此，它允许嵌套在比0小时更晚的时间开始。设置fine_input_stream = 0等效于第二个选项。
要运行real.exe进行嵌套运行，必须首先运行WPS并为所有嵌套创建数据。 假设WPS运行24小时，从2000年1月24日UTC开始，是两个域的嵌套案例。然后，应在WPS目录中生成以下文件：
met_em.d01.2000-01-24_12:00:00
met_em.d01.2000-01-24_18:00:00
met_em.d01.2000-01-25_00:00:00
met_em.d01.2000-01-25_06:00:00
met_em.d01.2000-01-25_12:00:00
met_em.d02.2000-01-24_12:00:00 
通常，仅需要嵌套输入文件的第一个时间段即可创建嵌套wrfinput文件。 将所有这些文件链接或移动到运行目录。
编辑namelist.input文件，并为所有相关变量设置正确的值（如前一页所述）（特别是将max_dom = 2设置为要运行的域的总数），以及物理选项。 输入以下内容以运行：
./real.exe >& real.out 
or
mpirun –np 4 ./real.exe
如果成功，这将为粗糙域和嵌套域创建所有输入文件。对于两个域的示例，将创建以下内容：
wrfinput_d01
wrfinput_d02
wrfbdy_d01
要运行WRF，请键入
./wrf.exe
or
mpirun –np 4 ./wrf.exe
如果成功，则模型应为域1和域2创建wrfout文件：
wrfout_d01_2000-01-24_12:00:00
wrfout_d02_2000-01-24_12:00:00

<a id=One_Way_ndown></a>

### 使用ndown的单向嵌套运行

WRF支持两个单独的单向嵌套选项。在本节中，单向嵌套定义为更精细的网格分辨率运行，在更粗糙的网格分辨率运行之后进行后续运行，其中，ndown程序在两个模拟之间运行。此较细网格运行的初始和横向边界条件是从粗网格运行获得的，输入来自更高分辨率的地面场（例如地形，土地利用等）和被遮盖的表面场（例如土壤温度和湿度）。执行此任务的程序是ndown.exe。
*Note：使用此程序需要编译代码以进行嵌套。
当使用单向嵌套时，粗网格对精网格的比率仅被限制为整数。建议使用小于或等于5的整数。还建议从粗网格运行中频繁输出（例如每小时）以提供更好的边界规范。在运行ndown时不要更改物理选项（可以在运行wrf时更改某些物理选项），并且如果打算使用ndown，请不要从注册表中删除字段。
步骤1：进行粗略的网格运行。
如上所述，这与任何单域WRF运行没有什么不同。尽可能多地输出历史记录文件，这将为下一次运行模型提供更好，更频繁的边界条件。
步骤二：为两个域运行geogrid.exe（提供geo_em.d01和geo_em.d02文件）和metgrid.exe（就像您要进行2向嵌套运行一样）。
这将为域1（met_em.d01。<date>）和域2（met_em.d02。<date>）生成WPS输出文件。
步骤3：为2个域运行real.exe。
此步骤的目的是摄取分辨率更高的陆地场和相应的被土地水掩盖的土壤场。
-将met_em *文件复制到将要运行real.exe的目录中。
-编辑namelist.input文件，更改“ max_dom = 2”，并确保为第2个域运行设置了第1列和第2列，并编辑了正确的开始时间和网格尺寸。
-运行real.exe。这将产生一个wrfinput_d01文件，一个wrfinput_d02文件和一个wrfbdy_d01文件。
-将wrfinput_d02文件重命名为wrfndi_d02。
步骤4：通过运行ndown.exe制作最终的细网格初始条件和边界条件文件
-从V3.2开始，必须在namelist.input的＆time_control部分中添加io_form_auxinput2 = 2才能成功运行ndown.exe。 （如果需要在运行ndown时优化垂直分辨率，请设置vert_refine_fact = integer（V3.2中的新增功能）。名称列表或过程中不需要其他更改。另一种优化垂直分辨率的方法是使用实用程序程序v_interp（有关详细信息，请参见“实用程序和工具”一章）。
-更改名称列表变量interval_seconds以反映粗略域模型运行的历史记录输出间隔。
-在运行ndown程序之前，请勿更改物理选项。
-运行ndown.exe，它使用来自粗网格wrfout文件的输入以及从上面的步骤3生成的wrfndi_d02文件。这将产生一个wrfinput_d02和wrfbdy_d02文件。
请注意，取决于选择的编译选项，程序ndown可以串行运行，也可以在MPI中运行。但是，必须构建ndown程序以支持嵌套。要运行该程序，请键入
./ndown.exe
or 
mpirun –np 4 ./ndown.exe 
步骤5：运行细网格WRF
-将wrfinput_d02和wrfbdy_d02分别重命名为wrfinput_d01和wrfbdy_d01。
-将原始wrfout_d01 *文件重命名（或移动）到其他文件（或其他目录），以免覆盖它们。
-编辑namelist.input，将所有精细网格域数据从第2列移至第1列，以便此运行仅适用于精细网格域。确保将time_step设置为符合精细网格域（通常为6 * DX）。如果您将来需要重复此过程，则在此步骤之前将namelist.input保存到其他内容可能是有益的。将新编辑的名称列表另存为namelist.input。

-可以在每次运行之间修改WRF模型的物理选项（ndown之前的WRF模型和ndown之后的WRF模型，但是在运行ndown时确实使用与第一次运行相同的物理特性），但通常情况下，陆地表面方案选项具有不同取决于方案的土壤深度数量。用户可以利用允许初始边界和横向边界同时使用潮湿数组和标量数组的功能（分别具有have_bcs_moist和have_bcs_scalar）。此选项仅在ndown处理之后的WRF模型运行期间使用。使用此选项，用户必须在预测之间保持微观物理选项相同。优点是，先前的WRF模型为所有微物理变量提供了实际的横向边界趋势，而不是简单的“零流入”或“零梯度流出”。
-为此网格运行WRF。
*请记住，此运行的输出将采用wrfout_d01 *的形式，但实际上将是域2的输出。这可能有助于重命名这些名称，以避免将来造成混淆。

为三个或更多域运行ndown.exe
可以使用ndown程序运行多个嵌套，但是此过程有点麻烦。由于编写代码的方式不同，它需要特定的文件名（特别是d01和d02），因此，准确地遵循以下步骤很重要：
注意：此示例用于嵌套到第三个域（总共3个域），并假定您已经具有上一次运行的wrfout_d01 *文件。
步骤A：为3个域运行geogrid.exe和metgrid.exe程序。您应该具有文件met_em.d01。<date>，met_em.d02。<date>和met_em.d03。<date>。
步骤B：为3个域运行real.exe。
-将met_em *文件复制到将要运行real.exe的目录中。
-编辑namelist.input文件，更改“ max_dom = 3”，并确保为第3个域运行设置了第1、2和3列，并编辑了正确的开始时间和网格尺寸。
-运行real.exe。这将生成wrfinput_d01，wrfinput_d02，wrfinput_d03文件和wrfbdy_d01文件。
-将wrfinput_d02文件重命名为wrfndi_d02。
步骤C：通过运行ndown.exe，使域02的网格初始条件和边界条件文件（请参阅上面的步骤4中的详细信息）
步骤D：运行域2 WRF（请参阅上面步骤5中的详细信息）。现在，您将拥有名为wrfout_d01 *的新文件，该文件将对应于域02。

步骤E：通过运行ndown.exe，使域03网格的初始条件和边界条件文件
-将wrfinput_d03文件重命名为wrfndi_d02（这是程序期望的名称）
-确保名称列表在＆time_control部分中仍具有io_form_auxinput2 = 2。
-更改名称列表变量interval_seconds以反映粗略域模型运行的历史记录输出间隔。
-在运行ndown程序之前，请勿更改物理选项。
-运行ndown.exe，它使用来自（新）粗网格wrfout文件和wrfndi_d02文件的输入。这将产生一个wrfinput_d02和wrfbdy_d02文件（实际上将对应于域03）。
步骤F：运行细网格（d03）WRF。
-将wrfinput_d02和wrfbdy_d02分别重命名为wrfinput_d01和wrfbdy_d01。
-将wrfout_d01 *文件重命名（或移动）到其他文件（或其他目录），以免覆盖它们（记得这些文件与d02相对应）。
-编辑namelist.input，将所有精细网格域数据从第3列移至第1列，以便此运行仅适用于精细网格域。确保将time_step设置为符合精细网格域（通常为6 * DX）。如果您将来需要重复此过程，则在此步骤之前将namelist.input保存到其他内容可能是有益的。将新编辑的名称列表另存为namelist.input。
运行wrf.exe后，您将拥有新的wrfout_d01 *文件。这些将对应于域03。如果需要添加更多的嵌套，请遵循相同的格式，并保持命名约定不变。

下一页的图总结了使用程序ndown进行单向嵌套运行的数据流。
 
 
<a id=Moving_Nested></a>

### 移动嵌套运行

WRF中允许两种类型的移动测试。在第一个选项中，用户在名称列表中指定嵌套移动。第二种选择是基于自动跟踪涡旋算法自动移动嵌套。此选项旨在跟随定义明确的热带气旋的运动。
要进行指定的移动嵌套运行，请选择正确的嵌套编译选项（选项“预设移动”）。要使用此选项，必须为分布式内存并行化选项（dmpar）配置代码以使用多个处理器。请注意，使用此选项编译的代码将不支持静态嵌套运行。要运行模型，仅需要粗网格输入文件。在此选项中，嵌套初始化是从粗略的网格数据定义的-不使用任何嵌套输入。除了将名称列表选项应用于嵌套运行之外，还需要在名称列表部分＆domains中添加以下内容：
num_moves：一个模型运行中可以进行的移动总数。任何域的移动都将计入该总数。当前最大值设置为50，但可以通过更改frame / module_driver_constants.F中的MAX_MOVES来更改。
move_id：嵌套ID的列表，每个动作一个，指示给定移动要移动的域。
move_interval：从运行开始到应该发生移动之间的分钟数。在指定的模型时间过去之后，嵌套将在下一个时间步移动。
move_cd_x，move_cd_y：网格点数和嵌套移动方向的距离（正数表示向东和北移动，负数表示向西和南移动）。
参数max_moves设置为50，但是可以根据需要在源代码文件frame / module_driver_constants.F中进行修改。
要进行自动移动嵌套运行，请在配置时选择“跟随涡流”选项。要使用此选项，必须为分布式内存并行化选项（dmpar）配置代码以使用多个处理器。再次注意，此编译将仅支持自动移动嵌套，而不同时支持指定的移动嵌套运行或静态嵌套运行。同样，不需要嵌套输入。如果要使用默认值以外的其他值，请在＆domains部分中添加并编辑以下名称列表变量：
vortex_interval：计算涡旋位置的频率，以分钟为单位（默认为15分钟）。
max_vortex_speed：与vortex_interval一起使用以计算新涡旋中心位置的搜索半径（默认值为40 m / sec）。
corral_dist ：允许移动的嵌套到达母域边界附近的粗网格单元数的距离（默认为8）。此参数可用于使伸缩嵌套居中居中，以便所有嵌套与风暴一起移动。
track_level ：跟踪涡旋的压力水平（以Pa为单位）。
time_to_move ：移动嵌套的时间（以分钟为单位）。当风暴仍然太弱而算法无法跟踪时，此选项可能会有所帮助。
当使用自动移动巢穴时，模型以标准输出文件（例如rsl.out.0000 ）中最小的平均海平面压力和最大的10 m风向转储涡流中心位置。输入' grep ATCF rsl.out.0000 '将以15分钟的间隔生成风暴信息列表：
ATCF	2007-08-20_12:00:00		20.37	-81.80	 929.7      133.9
ATCF 	2007-08-20_12:15:00		20.29	-81.76	 929.3      133.2
在这两种类型的移动嵌套运行中，嵌套的初始位置都是通过namelist.input 文件中的i_parent_start 和j_parent_start 指定的。
从V3.6开始，已添加了在移动的嵌套运行中合并高分辨率地形和土地使用输入的功能（Chen，Shuyi S.，赵薇，Mark A. Donelan，James F. Price，Edward J. Walsh，2007年） ：该CBLAST飓风计划和下一代完全耦合的飓风研究和预测大气波海洋模式。。公牛阿米尔流星志，88 ，311-317.doi：HTTP：//dx.doi。 org / 10.1175 / BAMS-88-3-311）。要激活此选项，
-在编译时，需要将环境变量TERRAIN_AND_LANDUSE设置为1。在cshell中，
setenv TERRAIN_AND_LANDUSE 1
然后配置和编译代码。
从WPS的V3.9开始，默认土地使用数据集已更改为MODIS。但是，此高分辨率数据集来自USGS，因此，为了使用此功能，应使用USGS准备您的土地利用数据。
-在运行时，在＆time_control中添加以下名称列表：
input_from_hires	= .true., .true.,
rsmas_data_path		= “terrain_and_landuse_data_directory”
自动移动巢穴最适合于发达的漩涡。

<a id=Analysis_Nudging></a>

### 分析数据推动运行（空中和/或地面）

照常使用WPS准备输入数据到WRF。如果需要在嵌套域中进行微调，请确保在WPS中处理了所有域的所有时间段。对于表面分析微调（版本3.1中的新增功能），需要在METGRID之后运行OBSGRID，并且它将输出wrfsfdda_d01 文件，WRF模型将为该选项读取该文件。
 
除了前面描述的其他选项外，在运行real.exe 之前，请设置以下选项（有关指导，请参见test / em_real / 目录中examples.namelist 中的名称列表）：
 
grid_fdda = 1
grid_sfdda = 1
 
像以前一样运行real.exe，这将除了创建wrfinput_d0*he wrfbdy_d01文件之外，还将创建名为'wrffdda_d0*' 的文件。其他网轻推名称列表在此阶段被忽略，但它是很好的做法，以填补他们都在一个运行前实际。特别是设定
gfdda_inname   =  “wrffdda_d<domain>”
gfdda_interval =  time interval of input data in minutes
gfdda_end_h    =  end time of grid-nudging in hours

sgfdda_inname   =  “wrfsfdda_d<domain>”
sgfdda_interval =  time interval of input data in minutes
sgfdda_end_h    =  end time of surface grid-nudging in hours

请浏览 http://www2.mmm.ucar.edu/wrf/users/wrfv3.1/How_to_run_grid_fdda.html 和 README.grid_fdda in WRF/test/em_real/以获得更多信息. 
V3.8中添加了不同的表面数据微调选项，并通过设置激活。
 
grid_sfdda = 2
 
该选项与选项1相似，可微调地表空气温度和水蒸气的混合比，但利用直接微动方法生成的趋势来限制表面感热和潜热通量，从而确保大气层与陆地表面之间的热力学一致性。这适用于YSU PBL和Noah LSM。（Alapaty等人，JAMC，2008年）
 
频谱微动是另一个高空微动选项。这只会有选择地微调较粗的比例，否则将以与网格微调相同的方式进行设置。此选项还会微调地理势高度。此处定义的波数是域中包含的波数，该数是被轻推的最大波数。
 
grid_fdda = 2
xwavenum = 3
ywavenum = 3

<a id=Observation_Nudging></a>

### 观测数据推动运行

除了使用WPS进行常规输入数据准备外，还需要站点观测文件。有关详细信息，请参见《观察轻推用户指南》和http://www2.mmm.ucar.edu/wrf/users/wrfv3.1/How_to_run_obs_fdda.html。WRF期望的观察文件名称对于域1 是OBS_DOMAIN101，对于域2 是OBS_DOMAIN201，依此类推。
通过＆fdda中的以下名称列表在模型中激活观察微调：
 
obs_nudge_opt= 1 
fdda_start= 0（以分钟为单位的obs起始时间）
fdda_end= 360（以分钟为单位的obss 结束时间） 
 
并在＆time_control中
auxinput11_interval_s = 180、180、180，（将间隔设置得足够小，这样将检查所有观察结果）
在test/em_real/目录中的examples.namelists 文件中寻找一个示例来设置其他讨厌的namelist变量。见该观察轻推用户指南，http://www2.mmm.ucar.edu/wrf/users/wrfv3.1/How_to_run_obs_fdda.html，并README.obs_fdda 在WRF /测试/ em_real / 获取更多信息。

<a id=Global_Run></a>

### 全球范围运行

WRF支持全局功能。要进行全局运行，请从名称列表模板namelist.wps.gloabl 开始运行WPS 。设置map_proj ='lat-lon' 并设置网格尺寸e_we 和e_sn，而无需在namelist.wps中设置dx 和dy 。该土工格栅程序将计算网格距离，和它们的值可以的全局属性节中找到geo_em.d01.nc 文件。键入ncdump –h geo_em.d01.nc 来查找网格距离，这是填写WRF的namelist.input 文件所必需的。x和y方向上的网格距离可能不同，但是最好将它们设置为相似或相同。WRF和WPS假定地球是一个球体，其半径为6370公里。对于网格尺寸使用什么没有限制，但是为了有效地使用WRF中的极点滤波器，东西方向的尺寸应设置为2 P * 3 Q * 5 R +1（其中P，Q和R是任何整数，包括0）。
照常运行其余WPS程序，但只能运行一个时间段。这是因为该域覆盖了整个地球，并且不再需要横向边界条件。 
照常运行程序real.exe ，并且只运行一个时间段。不需要横向边界文件wrfbdy_d01 。
将namelist.input.global 复制到namelist.input ，然后进行编辑。照常运行模型。
注意：由于这不是模型中的常用配置，因此请谨慎使用。并非所有的物理和扩散选项都已通过测试，某些选项可能不适用于极光滤镜。同样，正定和单调对流选项在全局运行中不适用于极坐标滤波器，因为极坐标滤波器会生成标量的负值。这也意味着，WRF-Chem不能在全局WRF设置中以正定和单调选项运行。
作为对全球lat-lon网格的扩展，还可以使用lat-lon网格来设置区域范围。为此，需要同时设置网格尺寸和网格距离（以度为单位）。假设地球是一个球体且半径为6370 km，土工格栅将再次计算出网格距离。在netCDF文件中找到以米为单位的网格距离，并将该值用于WRF的namelist.input 文件。

<a id=DFI_Run></a>

### 使用数字滤波器初始化（Digital Filter Initialization）运行

数字滤波器初始化（DFI）是V3中的新选项。这是一种消除初始模型不平衡的方法，例如通过表面压力趋势来衡量。当人们对0到6小时的模拟/预测感兴趣时，这可能很重要。在短期模型集成过程中（向前和向后）运行数字滤波器，然后开始预测。在WRF实施中，所有这些都在一个工作中完成。在V3.3版本中，DFI可用于具有并发嵌套且禁用反馈的多个域。
数据准备没有特殊要求。
从test / em_real /目录中的example.namelist 文件开始，查找以DFI的名称列表记录开头的部分：＆dfi_control，然后将其剪切并粘贴到namelist.input 文件中。编辑它以匹配您的案例配置（例如日期）。对于典型的应用程序，使用以下选项：
dfi_opt = 3（注意：如果要重新启动，则必须将其更改为0）
dfi_nfilter = 7（过滤器选项：Dolph）
dfi_cutoff_seconds = 3600（不应长于过滤器窗口）
对于时间规格，通常需要向后积分0.5到1小时，并向前积分一半时间。
如果选项dfi_write_filtered_input 设置为true，过滤wrfinput文件，wrfinput_initialized_d01 ，当您运行WRF会产生。
在版本3.2中，为DFI引入了恒定边界条件选项。要使用它，请在＆bdy_control中设置constant_bc = 1
如果DFI使用不同的时间步长，则可以使用time_step_dfi 进行设置。

<a id=SST_Update></a>

### 使用sst_update选项

WRF模型物理学无法预测海面温度，植被比例，反照率或海冰。对于长时间的仿真，该模型提供了一种替代方法，可以读取随时间变化的数据并更新这些字段。为了使用此选项，必须访问随时间变化的SST和海冰场。土工格栅程序可提供十二个月的植被分数和反照率值。通过WPS处理完这些字段后，可以在运行程序real.exe 和wrf.exe 之前激活名称列表记录＆time_control中的以下选项：
io_form_auxinput4 	= 2
auxinput4_inname 	= “wrflowinp_d<domain>” (created by real.exe)
auxinput4_interval 	= 360, 360, 360,

and in &physics

sst_update = 1

请注意，此选项不适用于sf_ocean_physics选项。

<a id=bucket_mm_and_J></a>

### 使用bucket_mm和bucket_J选项

这些选项适用于长时间的模拟降雨累积和辐射预算累积项（RAINC，RAINNC，ACSWUPT，ACLWDNBC等）。由于具有32位精度，因此将小数加到非常大的数会随着累加项的增加而降低精度。对于几天到几周的模拟，通常可以进行累加，但是对于几个月到几年的累加，则具有截断加法的效果，尤其是小的量可能会被清零。
激活这些选项后，该术语的一部分将以整数存储，每次达到存储桶值时，该整数将递增1，因此我们有两个术语-RAINNC和I_RAINNC，其中RAINNC现在仅包含余数。从总计= RAINNC + bucket_mm * I_RAINNC的输出中检索总计。合理的铲斗值可以基于每月累积量（例如100毫米）。总降水等于RAINC + RAINNC，其中
Total RAINNC = RAINNC+bucket_mm*I_RAINNC
Total RAINC = RAINC+bucket_mm*I_RAINC
辐射累积项（例如ACSWUPT）以焦耳/ m ^ 2为单位，因此模拟周期内的平均值是差除以时间之间的差，得出W / m ^ 2。
bucket_J选项适用于这些术语，基于月累积量的典型值为1.e9J。此处的总和由（ACSWUPT示例-其他辐射项遵循相同的方程式概念）给出：
总计= ACSWUPT + bucket_J * I_ACSWUPT

<a id=Adaptive_Time_Stepping></a>

### 使用自适应时间步长

自适应时间步长是一种最大化模型可以使用的时间步长，同时保持模型数值稳定的方法。基于整个域的水平和垂直稳定性标准（称为Courant-Friedrichs-Lewy（CFL）条件）调整模型时间步长。以下一组值通常会很好地工作。
 
use_adaptive_time_step = .true.
step_to_output_time = .true。（但嵌套域可能仍会在所需时间写入输出。请尝试使用adjust_output_times = .true.来弥补这一点。）
target_cfl = 1.2、1.2、1.2 ，
max_step_increase_pct = 5、51、51 （很大的百分比值）对于嵌套，允许嵌套的时间步长具有更大的调整自由）
starting_time_step = 实际值或-1（表示开始时为4 * DX）
max_time_step：对所有域使用固定值，例如8 * DX
min_time_step：对所有域使用固定值，例如3 * DX
Adaptation_domain ：哪个域正在驱动自适应时间步长
 
也查看描述these options，在名称列表清单5-43页。

<a id=Stochastic_Parameterization_Schemes></a>

### 随机参数化方案

随机参数化套件包括许多随机参数化方案，其中一些已被广泛使用，有些是为非常特定的应用开发的。通过在每个时间步长上对每个成员施加较小的扰动，可将其用于表示集成仿真中的模型不确定性。这些方案中的每一个都生成其自己的随机扰动场，其特征在于空间和时间相关性以及在名称列表记录和存储（自版本3.6起）中定义的总体扰动幅度。
随机扰动会在每个时间步在父域上生成，并且默认情况下会插值到嵌套域。名称列表设置确定这些扰动在哪些域上应用。通过设置例如sppt = 0,1,1，扰动将仅应用于嵌套域。
由于该方案使用库FFTPACK中提供的快速傅立叶变换（FFT），因此我们建议每个方向上的网格点数量应为小质数的乘积。如果网格点的数量在至少一个方向上是大素数，则计算成本可能会大大增加。
随机扰动字段（rand_perturb = 1） 
此选项为用户实现的应用程序生成3-D高斯随机扰动字段。扰动字段在历史文件中保存为  rand_pert （从3.7版开始可用）。
随机扰动的物理趋势（SPPT）（sppt = 1）
随机模式用于扰动潜在温度，风和湿度的累积物理趋势（微物理除外）。有关WRF实施的详细信息，请参见Berner等人，2015（http://journals.ametsoc.org/doi/abs/10.1175/MWR-D-14-00091.1）。扰动字段以rstoch的形式保存在历史记录文件中（从3.9版开始可用）。
随机动能反向散射方案（SKEBS）（skebs = 1）
使用随机模式来扰动潜在温度和旋转风分量。扰动字段分别在u，v和θ 的历史文件中另存为ru_tendf_stoch，rv_tendf_stoch和rt_tendf_stoc h 。  有关WRF实施的详细信息，请参见Berner等人，2011 http://journals.ametsoc.org/doi/abs/10.1175/2010MWR3595.1）和http://www2.mmm.ucar.edu/wrf/users/ docs / skebs_in_wrf.pdf。风的扰动与动能反向散射率的平方根成正比，温度的扰动与势能反向散射率成正比（详见http://www2.mmm.ucar.edu/wrf/users/docs/skebs_in_wrf .pdf。
默认参数用于中纬度的天气尺度尺度扰动。Romine等人讨论了调整策略。2014（http://journals.ametsoc.org/doi/citedby/10.1175/MWR-D-14-00100.1）和Ha等。2015（http://journals.ametsoc.org/doi/10.1175/MWR-D-14-00395.1）
随机扰动参数方案（SPP = 1）
随机模式用于扰动所选物理软件包中的参数，即GF对流方案，MYNN边界层方案和RUC LSM。可以通过设置spp_conv = 1，spp_pbl = 1 或spp_lsm = 1 来实现对单个物理包的参数摄动。有关实现的详细信息，请参见Jankov等。（http://journals.ametsoc.org/doi/abs/10.1175/MWR-D-16-0160.1）。扰动字段在历史文件中另存为  pattern_spp_conv，pattern_spp_pbl，pattern_spp_lsm 。（从3.9版开始可用）。

n.边界条件的随机扰动（perturb_bdy） 
对于perturb_bdy = 1 ，随机随机场用于扰动风和潜在温度的边界趋势。所述perturb_bdy 独立的SKEBS和作为这样的选项运行可能具有或不具有SKEB方案，该方案仅在内部网格操作运行。但是，选择perturb_bdy = 1 将需要生成域大小的随机数组，因此计算时间可能会增加。    
对于perturb_bdy = 2 ，使用用户提供的模式来扰动边界趋势。数组被初始化并调用：field_u_tend_perturb，field_v_tend_perturb，field_t_tend_perturb 。这些数组将需要在share / module_bc.F的spec_bdytend_perturb或dyn_em / module_bc_em.F的spec_bdy_dry_perturb中填充所需的模式 

对WRF-CHEM中边界趋势的随机扰动（perturb_chem_bdy）
由选项rand_perturb = 1 （请参见上文）创建的随机模式用于扰动WRF-CHEM中的化学边界趋势。对于此应用程序，应在WRF编译时编译WRF-Chem。
所述perturb_chem_bdy 独立的选项运行rand_perturb 并且因此可以具有或不具有运行rand_perturb 方案，该方案仅在内部网格操作。但是，选择perturb_bdy_chem = 1 将需要生成域大小的随机数组以在横向边界区域中应用扰动，因此计算时间可能会增加。使用have_bcs_chem = 运行WRF-Chem时。是的。在＆chem中，从wrfbdy_d01读取的化学LBC被rand_perturb = 1 （从3.7版开始）创建的随机模式干扰。   

<a id=Run_Time_IO></a>

### Run-time IO

随着WRF 3.2版的发布, IO决策现在可以作为运行时run-time选项进行更新。以前，对IO的任何修改（例如，哪个变量与哪个流相关联）都是通过注册表进行处理的，而对注册表的更改总是需要一个clean –a, configure, h和compile循环。这种编译时机制仍然可用，这是定义大多数WRF IO的方式。但是，如果用户希望从各种流中添加（或删除）变量，则可以选择使用该功能。
首先，用户让WRF模型知道IO的运行时修改信息所在的位置。这是一个文本文件（my_file_d01.txt），用于每个域，在time_control 名称列表记录中的namelist.input 文件中定义。
&time_control
iofields_filename = “my_file_d01.txt”, “my_file_d02.txt”
ignore_iofields_warning = .true.,
/
 
文本文件的内容将流ID（0为默认历史记录和输入）与变量以及是否要添加或删除字段相关联。状态变量必须已经在注册表文件中定义。以下是一些示例：
-：h：0：RAINC，RAINNC
将从标准历史记录文件中删除字段RAINC和RAINNC。
 
+：h：7：RAINC，RAINNC
将字段RAINC和RAINNC添加到输出流＃7。
可用的选项有：
              + 或- ，添加或删除变量
              0-24 ，整数，哪个流
              i 或h ，输入或历史
   注册表中的字段名称-这是引号中的第一个字符串。注意：不包括字段名称之间的任何空格。
不必从一个流中删除字段以将其插入到另一个流中。可以在多个流中具有相同的字段。
如果您有兴趣将变量输出到新流中（即不是默认的历史记录流0），那么以下名称列表变量也将是必需的（流7的示例）：
auxhist7_outname =“您的流名称_d <domain> _ <date>”
auxhist7_interval = 360、360，
frame_per_auxhist7 = 1，1，
io_form_auxhist7 = 2
名称列表变量ignore_iofields_warning 告诉程序如果在这些用户指定的文件中遇到错误，该怎么办。默认值.TRUE。是打印警告消息，但继续运行。如果设置为.FALSE。，如果这些用户指定的文件中有错误，程序将中止。
请注意，必须是可选IO一部分（输入或输出流）的任何字段，都必须已在注册表中声明为状态变量。指定为运行时IO选择的变量的名称时，应格外小心。在文本文件（在namelist.input文件中定义）中使用的变量的“名称”是注册表文件中带引号的字符串。大多数WRF变量具有与WRF源代码内使用的变量名称相同的字符串（注册表文件中的第3列，未加引号，而不是要使用的字符串），并且该变量的名称与netCDF文件（注册表文件中第9列，带引号，这是要使用的字符串）。  

<a id=Output_Diagnostics></a>

### 输出诊断

1.时间序列输出。要激活该选项，必须在WRF运行目录中存在一个名为“ tslist ” 的文件。该tslist 文件包含由他们的纬度和经度一个简短的描述，并为每个位置的缩写一起定义位置的列表。示例文件如下所示：
  
＃-----------------------------------------------＃
名称的＃24个字符| pfx | 拉特| LON |
＃-----------------------------------------------＃
哈雷特角哈特-72.330 170.250
麦克默多站mcm -77.851 166.713
 
文件中的前三行被视为标题信息，并被忽略。给定tslist 文件，对于模型域内的每个位置（粗略或嵌套），将在每个模型时间步长包含时间序列变量的文件写入名称pfx.d <domain> .TS ，其中pfx是指定的前缀tslist文件中的位置。时间序列位置的最大数量由名称列表记录＆domains 中的名称列表变量max_ts_locs 控制。默认值为5。时间序列输出包含表面上的选定变量，包括2米温度，蒸汽混合比，10米风分量，u和v，旋转到地球坐标等。有关时间的更多信息系列输出可以在WRF / run / README.tslist中找到。
 
从V3.5开始，除表面变量外，还将输出相对于地球的U和V的垂直剖面，势能温度，水蒸气和地势高度。输出中的默认级别数为15，但可以使用名称列表变量max_ts_level 进行更改。
 
2.压力水平输出。这可以通过添加名称列表记录＆diags 并设置p_lev_diags = 1 来激活。该选项可以在多个压力水平下输出U，V，风速，T，露点T，RH和地势高度。
 
＆diags
p_lev_diags = 1
num_press_levels = 4
press_levels = 85000，70000，50000，20000 ，
 
输出进入辅助输出流23，因此应在＆time_control中设置以下内容：
 
auxhist23_interval = 360、360，
frames_per_auxhist23 = 100、100，
io_form_auxhist23 = 2
 
3. nwp_diagnostics = 1 ＆time_control 。对流风暴诊断。此选项可输出最大10 m的风速，在2-5 km层中的最大螺旋度，在400 mb以下的上下气流中的最大垂直速度，在2-5 km层中的平均垂直速度以及在历史记录之间的时间窗口中的最大列级输出时间。多余的字段将打印在历史记录文件中。还应该打开do_radar_ref 。
 
4. output_diagnostics = 1在＆time_control中。气候诊断。此选项输出36个表面诊断变量：最大值和最小值，出现最大值和最小值的时间，平均值，T2，Q2，TSK，U10，V10、10 m风速，RAINCV，RAINNCV的平均值的标准偏差（最后两个是时差雨）。输出进入辅助输出流3，因此需要以下内容：
 
auxhist3_outname =“ wrfxtrm_d <域> _ <日期>”
auxhist3_interval = 1440，1440，
frames_per_auxhist3 = 100、100，
io_form_auxhist3 = 2
 
由于此选项计算每日的最大值和最小值等，因此建议以每日间隔重新启动。
 
5. do_avgflx_em = 1 ＆DYANMICS 。此选项为下游传输模型输出历史时间平均，柱压耦合的U，V和W。如果使用Grell型方案，则do_avg_cugd = 1 将输出时间平均对流质量通量。
 
6. ＆afwa中的afwa_diag_opt = 1 。用于打开AFWA提供的天气诊断的主要控制选项。输出进入辅助流2。（请参见http://www2.mmm.ucar.edu/wrf/users/docs/AFWA_Diagnostics_in_WRF.pdf的完整文档）。注意：这些选项不能与OpenMP一起使用。
 
afwa_ptype_opt = 1 降水类型              
afwa_severe_opt = 1 恶劣天气诊断             
afwa_vil_opt = 1 垂直积分液体                           
afwa_radar_opt = 1 雷达             
afwa_icing_opt = 1 糖衣             
afwa_vis_opt = 1 可见度                           
afwa_cloud_opt = 1 云             
afwa_therm_opt = 1 热指数             
afwa_turb_opt = 1 湍流             
afwa_buoy_opt = 1 浮力             
 
7. solar_diagnostics = 1 in ＆diags 。太阳预报诊断。此选项输出太阳天顶角，净度指数，2-D最大云量，水蒸气路径，液体水路径，冰水路径，雪水路径，总水路（液体+冰+雪），液体云有效半径，冰有效半径，积雪有效半径，液态云光学厚度，结冰光学厚度，积雪光学厚度，云底高度和云顶高度。对于液体和冰的变量，计算了“总”水路，有效半径和光学厚度，其中“总”变量说明了亚网格水凝物。如果激活了solar_diagnostics，并且存在tslist，则将这些相同的变量写入各自的时间序列文件。
 
8.其他&physics
do_radar_ref = 1：使用模型中特定于微物理学的参数来计算雷达反射率。适用于mp_physics = 2,4,6,7,8,10,14,16。
 
prec_acc_dt = 60：输出降水量的时间间隔（从积云和微物理学方案中降雨，从微物理学方案中积雪）（以分钟为单位）。

<a id=Hydro></a>

### WRF-Hydro

这是V3.5中的一项新功能。它将WRF模型与水文过程（例如路由和航道）耦合在一起。使用WRF-Hydro需要使用环境变量WRF_HYDRO 进行单独的编译。在c-shell环境中
setenv WRF_HYDRO 1
在进行“ 配置”和“ 编译”之前。编译完WRF后，将文件从hydro / Run / 目录复制到您的工作目录（例如test / em_real /）。还需要单独准备的土工格栅文件。请参考以下网站以获取详细信息：http : //www.ral.ucar.edu/projects/wrf_hydro/。（来自于伟）

<a id=IO_Quilting></a>

### 使用IO Quilting

此选项允许留出几个处理器以仅负责输出。如果域大小很大，和/或与在输出时间之间集成模型所花费的时间相比，写输出时间所花费的时间变得很重要，那么它可能是有用且性能友好的。设置选项有两个变量：
 
nio_tasks_per_group ：每个IO组使用多少处理器进行IO缝。             
通常，为此目的一个或两个处理器就足够了。
nio_groups ：IO的多少个IO组。默认值为1。                           
 
*注意：此选项仅用于wrf.exe。它不适用于real或ndown。

<a id=Physics_Suites></a>

### 使用物理套件

从3.9版开始，引入了使用物理套件的选项。当前有2个可用的已批准套件（“ CONUS”和“ tropical”），它们需要在namelist.input中使用单行规范，其中包含一组经过严格测试并显示出良好且合理的结果的物理选项。 
 
要使用这些选项之一，只需在＆physics名称列表记录中的namelist.input中设置“ physics_suite”参数，例如，
 
physics_suite ='CONUS'
 
并将为所选套件设置打包的物理选项（特别是mp_physics，cu_physics，ra_lw_physics，ra_sw_physics，bl_pbl_physics，sf_sfclay_physics和sf_surface_physics）。在运行时，模型向rsl文件打印将在仿真中使用的物理方案的摘要，如下所示（注意：这是2域运行的示例。假定所有嵌套都使用相同的物理原理）除非用户明确地覆盖了这些选项，否则请参见下面的示例）：
 
physics_suite = 'tropical'        physics_suite = 'CONUS'
mp_physics =         6,  6        mp_physics =         8, 8
cu_physics =        16, 16        cu_physics =         6, 6
ra_lw_physics =      4,  4        ra_lw_physics =      4, 4
ra_sw_physics =      4,  4        ra_sw_physics =      4, 4 
bl_pbl_physics =     1,  1        bl_pbl_physics =     2, 2
sf_sfclay_physics = 91, 91        sf_sfclay_physics =  2, 2
sf_surface_physics = 2,  2        sf_surface_physics = 2, 2  
 
只需将特定参数添加到名称列表中，即可覆盖上述任何选项。例如，如果您想使用CONUS套件，但想为域3关闭cu_physics：
 
physics_suite ='CONUS'
cu_physics = -1，-1、0
 
如果您想使用CONUS套件，但想使用其他cu_physics选项，并为域3关闭cu_physics：
 
physics_suite ='CONUS'
cu_physics = 2，2，0
 
<a id=HVC></a>

### 混合垂直坐标

从版本3.9开始，该选项可用于使用地形跟随（TF）垂直坐标（自最初发布以来已用于欧拉质量模型的WRF模型中的垂直坐标）或混合垂直坐标（HVC） 。在此，HVC是在地面附近跟随地形并在预定义用户级别变为等压的坐标。通过选择混合垂直坐标，干压力现在定义为： 
P DRY （i，j，k）= B（k）（P DRY SFC （i，j）– P TOP ）+（（k）– B（k））（P 0 – P TOP ）+ P TOP
其中B（k）字段是内部计算的一维加权数组。
当B（k）≡ （k）时，此定义为当前TF坐标。
当B（k）≡ ，该定义为等压的坐标系。
 
垂直值，其中B（k）的阵列转换到同量异序，Ç ，确定有多少的层（向下从模型盖）是同量异位的。ETAC的默认值是在注册表文件中设置的，可以在全球范围内安全使用。图5.1显示了在ETAC的多个值下，坐标面从TF到HVC的过渡。
 
图5.1坐标曲面从地形跟踪（TF）到等压线的过渡是the临界值的函数，在该临界值下，用户要求实现等压线曲面。 当从“加权项B（）”轴上的任何值跟踪一条水平线时，就可以看出TF与HVC系统的基本属性。 例如，在= 0.2时，TF系统中的模型坐标“平坦度”与在VCC = 0.4时，当H = 0.6的近似值时，HVC系统中的模型坐标相同。

给出了等压坐标（图5.2a），地形坐标（图5.2b）和混合坐标（图5.2c）的表面的垂直位置，并带有简单的2d横截面。显示的是大气高度（m）和压力。
   
图5.2三本横截面图显示的垂直位置表面，用于一个给定的模型盖（25公里大约为25帕斯卡）和用于给定Ç = 0.2。

在V4.0中，此选项成为默认选项。设置hybrid_opt = 0 选项将使模型返回到原始的地形跟随坐标。
重要的是real.exe 和wrf.exe 程序都必须以相同的hybrid_opt值运行。
将HVC数据发送到后处理器时，用户必须小心。后处理器必须知道干压的新定义。最好将静水压力（P_HYD）或总压力（PB + P）用于诊断和垂直插值。
 
<a id=Multiple_Lateral_Condition></a>

### 使用多个横向条件文件

为了在实时场景中加快对横向边界条件的预处理，该模型可以创建多个横向条件文件，但是可以通过编译选项来完成（在configure.wrf文件的ARCH_LOCAL宏中添加-D_MULTI_BDY_FILES_）。这样就可以在周围时间段可用时立即创建边界条件文件，因此可以使模型更快地开始仿真。从V4.2开始，可以通过运行时选项来实现。要使用此选项，需要在namelist.input文件中添加以下选项： 
＆time_control
bdy_inname =“ wrfbdy_d <域> _ <日期>”
 
和
 
＆bdy_control
multi_bdy_files = .true。

生成的文件将如下所示（每6小时一次的数据间隔）：
wrfbdy_d01_2000-01-24_12：00：00
wrfbdy_d01_2000-01-24_18：00：00
wrfbdy_d01_2000-01-25_00：00：00
wrfbdy_d01_2000-01-25_06：00：00
 
<a id=Examples_namelists></a>

### 各种应用的namelist示例

这里提供了一些物理选项集（加上模型顶部和垂直级别的数量）以供参考。它们可以为在您的应用程序中测试模型提供一个很好的起点。另请注意，其他因素也会影响结果；例如，域设置，垂直模型级别的分布以及输入数据。
a.1-4公里的网格距离，需要运行1至3天（用于2013年美国NCAR春季实时对流预报以及2015年至2017年3 km合奏，这就是“圆锥曲线” （不含积算方案的物理套件）：
mp_physics                          = 8,
ra_lw_physics                       = 4,
ra_sw_physics                       = 4,
radt                                = 10,
sf_sfclay_physics                   = 2,
sf_surface_physics                  = 2,
bl_pbl_physics                      = 2,
bldt                                = 0,
cu_physics                          = 0,
ptop_requested                      = 5000,
e_vert                              = 40,

b．网格距离为10 – 20 km，运行1至3天（例如，美国NCAR每日实时运行）：
mp_physics = 8，
ra_lw_physics = 4，
ra_sw_physics = 4，
radt = 15，
sf_sfclay_physics = 1，
sf_surface_physics = 2，
bl_pbl_physics = 1，
bldt = 0，
cu_physics = 3，
cudt = 0，
ptop_requested = 5000，
e_vert = 39，

C。寒冷地区10 – 30 km的网格尺寸（例如，在NCAR的南极中尺度预测系统中使用）：
mp_physics = 4，
ra_lw_physics = 4，
ra_sw_physics = 2，
radt = 15，
sf_sfclay_physics = 2，
sf_surface_physics = 2，
bl_pbl_physics = 2，
bldt = 0，
cu_physics = 1，
cudt = 5，
fractional_seaice = 1，
seaice_threshold = 0.0，
ptop_requested = 1000，
e_vert = 44，

d。飓风应用（例如NCAR的实时飓风在2012年使用的36、12和4 km嵌套）：mp_physics = 6，
ra_lw_physics = 4，
ra_sw_physics = 4，
radt = 10，
sf_sfclay_physics = 1，
sf_surface_physics = 2，
bl_pbl_physics = 1，
bldt = 0，
cu_physics = 6，（仅在36/12 km网格上）
cudt = 0，
isftcflx = 2
ptop_requested = 2000，
e_vert = 36，

e．10 – 30 km网格大小的区域气候案例（例如用于NCAR的区域气候运行）：
mp_physics = 6，
ra_lw_physics = 3，
ra_sw_physics = 3，
radt = 30，
sf_sfclay_physics = 1，
sf_surface_physics = 2，
bl_pbl_physics = 1，
bldt = 0，
cu_physics = 1，
cudt = 5，
sst_update = 1，
tmn_update = 1，
sst_skin = 1，
bucket_mm = 100.0，
bucket_J = 1.e9，
ptop_requested = 1000，
e_vert = 51，
spec_bdy_width = 10，
spec_zone = 1，
relax_zone = 9，
spec_exp = 0.33，

<a id=Check_Output></a>

### 检查输出文件

模型运行完成后，优良作法是快速检查几件事。
如果使用MPI在多个处理器上运行模型，则应该有多个rsl.out。* 和rsl.error。* 文件。
输入“ tail rsl.out.0000 ”以查看是否获得“ SUCCESS COMPLETE WRF ”。这很好地表明该模型已成功运行。
名称列表选项将写入一个单独的文件：namelist.output 。
使用netCDF命令检查写入wrfout * 文件的输出时间：
  ncdump –v时间wrfout_d01_yyyy-mm-dd_hh：00：00
查看rsl.out.0000 文件或其他标准输出文件。此文件记录了为一个模型时间步进行计算并写入一个历史记录并重新启动输出文件所花费的时间：
Timing for main: time 2006-01-21_23:55:00 on domain  2:    4.91110 elapsed seconds.
Timing for main: time 2006-01-21_23:56:00 on domain  2:    4.73350 elapsed seconds.
Timing for main: time 2006-01-21_23:57:00 on domain  2:    4.72360 elapsed seconds.
Timing for main: time 2006-01-21_23:57:00 on domain  1:   19.55880 elapsed seconds.
and
Timing for Writing wrfout_d02_2006-01-22_00:00:00 for domain 2: 1.17970 elapsed seconds.
Timing for main: time 2006-01-22_00:00:00 on domain 1: 27.66230 elapsed seconds.
Timing for Writing wrfout_d01_2006-01-22_00:00:00 for domain 1: 0.60250 elapsed seconds.

<a id=Trouble_Shooting></a>

### 故障排除

- 如果模型很快中止，则可能是计算机内存不足以运行特定配置，或者输入数据存在严重问题。对于第一个潜在问题，请尝试输入' unlimit '或' ulimit -s unlimited '，以查看是否可以获得更多的内存和/或堆栈大小。         
- 对于OpenMP（Smpar编译的代码），需要将堆栈大小设置为大，但不限制为无限。无限的堆栈大小可能会使计算机崩溃。         
- 要检查输入数据是否有问题，请使用ncview或其他netCDF文件浏览器检查wrfinput文件中的字段。         
- 看到的另一个常见错误是“ module_configure：initial_config：读取名称列表时出错”。这是来自模型的错误消息，说namelist.input 文件中存在错误和错别字。谨慎编辑namelist.input 文件。如果不确定，请始终从可用的模板开始。V3错误消息中提供了发生名称列表读取错误的名称列表记录，它应有助于识别错误。         
- 如果模型不能完成运行，一种可能性是该模型可能在数值上变得不稳定，这意味着用于及时求解模型的时间步长太大，无法获得稳定的解决方案。即使遵守设置模型时间步长的标准规则（在物理空间中以千米为单位约为6 * DX），模型域的其他配置也可能会影响结果。例如，如果一个模型层很薄，或者使用很大的区域，并且该区域的角可能具有非常大的地图比例因子，则这会使等效地球距离减小到比模型网格大小小得多。可以通过在标准输出/错误文件（例如rsl文件）中搜索CFL打印来找出是否是这种情况：         
grep cfl rsl.error。* 或grep cfl wrf.out

你可能会看到以下内容：
5 points exceeded cfl=2 in domain            1 at time   4.200000  
  MAX AT i,j,k:          123          48          3 cfl,w,d(eta)= 4.165821 
21 points exceeded cfl=2 in domain            1 at time   4.200000  
  MAX AT i,j,k:          123          49          4 cfl,w,d(eta)= 10.66290

     i，j，k的最大值：123 49 4 cfl，w，d（eta）= 10.66290 发生这种情况时，请考虑使用名称列表选项w_damping 和/或减少时间步长。  
	 
<a id=Physics_Dynamics></a>

### 物理与动力学选项

物理选项physics options
WRF提供了多种物理选项，可以以任何方式进行组合。这些选项的范围通常从简单高效，复杂到计算成本更高，从新开发的方案到成熟的方案（例如当前操作模型中的方案）。
每个主要的WRF版本都会有不同的选择，但是这里我们将概述WRF版本3和4中提供的选项。
1.微物理方案（mp_physics）
a.	Kessler暖云方案：一种理想的云模型研究中常用的暖云降水方案（即无冰）（mp_physics = 1）。
b.	Purdue Lin方案：具有冰，雪和up过程的复杂方案，适用于真实数据的高分辨率模拟（2）。
c.	WRF单矩3类方案：一种简单，有效的方案，具有适用于中尺度网格大小的冰雪过程（3）。
d.	WRF单矩5级方案：（c）的稍微复杂一些的版本，它允许进行混合相过程和过冷的水（4）。
e.	Eta微观物理学：NCEP模型中的操作微观物理学。具有诊断混合阶段过程的简单有效方案。对于精细分辨率（<5 km），请使用选项（5），对于较粗分辨率，请使用选项（9 5）。
f.	WRF单矩6类方案：一种具有冰，雪和毛刺过程的方案，适用于高分辨率模拟（6）。
g.	戈达德4冰微物理学方案（7）分别预测冰雹和graupel，为辐射提供有效半径。取代了V4.1中较旧的Goddard方案。
h.	New Thompson et al方案：一种适用于高分辨率模拟的具有冰，雪和毛刺过程的新方案（8）。这增加了雨水浓度集中并从3.0版中的雨水浓度更新方案。3.1版的新功能。
i.	Milbrandt-Yau Double-Moment 7级方案（9）。该方案包括单独的带有双时云，雨，冰，雪，graupel和冰雹的冰雹和graupel类别。3.2版中的新功能。（注意：请勿在V3.6和V3.6.1中使用此方案。
j.	莫里森双矩方案（10）。双重时刻的冰，雪，雨和雨滴，用于云解析模拟。3.0版中的新功能。
k.	CAM V5.1 2矩5级方案。
l.	斯托尼布鲁克大学（Y. Lin）计划（13）。这是一个5类方案，其边缘强度预计可解释混合相过程。版本3.3中的新增功能。
m.	 WRF Double-Moment 5类方案（14）。这个计划有双时雨。云和CCN用于温暖的进程，但与WSM5类似。3.1版的新功能。
n.	WRF Double-Moment 6级方案（16）。这个计划有双时雨。云和CCN用于温暖的进程，但与WSM6类似。3.1版的新功能。
o.	NSSL 2矩方案（17、18）。自3.4版以来的新功能，这是针对云滴，雨滴，冰晶，雪，gra和冰雹的两步方案。它还可以预测平均graupel颗粒密度，这可使graupel跨越从冻结液滴到低密度graupel的范围。还有一个附加选项可以预测云凝结核浓度（CCN，选项18）的浓度（适用于理想的模拟）。该方案旨在用于研究应用中的云解析模拟（dx <= 2km）。从V3.5开始，又增加了两个单矩方案（19和21）。选项19是NSSL方案的单机版，选项21与Gilmore等类似。（2004）。选件22（V3.7中的新增功能）是不加冰雹的两时制（选件17）。
p.	WSM7（24）。与WSM6一样，但是添加了一个冰雹类别。V4.1中的新功能。
q.	WDM7（26）。与WDM6一样，但添加了冰雹类别。V4.1中的新功能。
r.	汤普森气雾剂（28）。该方案考虑了对水和冰友好的气溶胶。气候数据集可用于指定气溶胶变量的初始条件和边界条件（Thompson和Eidhammer，2014年，JAS。）（版本3.6中的新增功能）。在版本4.0中添加了表面除尘方案。
s.	自版本3.6起，HUJI（以色列希伯来大学，以色列）的光谱箱微物理学有完整（32）和“快速”（30）版本可用。
t.	带有CESM气雾剂（40）的莫里森双矩方案：必须与MSKF积云方案一起使用。版本4.0中的新增功能。
u.	P3（Morrison和Milbrandt）（50，51，52）：预测的粒子特性方案。它具有一个代表冰，雪和，的组合的冰类别，并且还带有边缘冰块和边缘冰量的预测数组。双时雨和冰（50）。P3-nc（51）：与P3一样，但添加了依赖于超饱和度的活化作用和双矩云水。V3.9中的新功能。P3-2ice（52）：与P3-nc中一样，但有两个冰块阵列。V4.0中的新功能。
v.	Jensen ISHMAEL（55）：该方案可预测冰晶生长中的颗粒形状和习惯。V4.1中的新功能。
 
2.1长波辐射（ra_lw_physics）
a.	RRTM方案（ra_lw_physics = 1）：快速辐射传输模型。使用查找表来提高效率的准确方案。解释了多个波段和微物理学物种。对于痕量气体，
CO 2 的体积混合比值为330e-6，N 2 O = 0。CH 4 = 0。在V3.5之前的代码中；在V3.5中，CO 2 = 379e-6，N 2 O = 319e-9和CH4 = 1774e-9。有关时变选项，请参见第2.3节。
b.	GFDL方案（99）：Eta工作辐射方案。具有二氧化碳，臭氧和微物理效应的较旧的多频带方案。
c.	CAM方案（3）：来自CCSM中使用的CAM 3气候模型。允许浮质和微量气体。它每年使用CO 2 ，常数为N 2 O （311e-9）和CH 4 （1714e-9）。有关时变选项，请参见第2.3节。
d.	dRRTMG方案（4）：版本3.1中添加了RRTM的新版本。它包括随机云重叠的MCICA方法。对于主要痕量气体，CO 2 = 379e-6（2005年有效），N 2 O = 319e-9，CH 4 = 1774e-9。有关时变选项，请参见第2.3节。在V3.7中，作为选项24引入了快速版本。从V4.2开始，CO 2 值被以下年份的函数所代替：CO2（ppm）= 280 + 90 exp（0.02 *（2000年））与观测值相比，在1920年代和1960年代误差约为4％，在2000年之后误差约为1％。
e.	戈达德计划（5）。来自简单气候的高效，多波段臭氧。设计用于与Goddard微物理学粒子半径信息一起运行。在V4.1中更新。
f.	傅留谷方案（7）。多波段，云和云分数效应，气候和示踪气体产生的臭氧剖面。CO 2 ＝ 345e-6。3.4版的新功能。
g.	RRTMG-K方案（14）：Baek（2017）改进的RRTMG方案版本，装有G的McICA的修订辐射包和两流近似法：全球天气预报模型中的性能评估，J. Adv。模型。Earth Syst。，9，doi：10.1002 / 2017MS000994）。V4.0中的新功能。 

2.2短波辐射（ra_sw_physics）
a.	Dudhia方案：简单的向下积分，可以有效地吸收云层，以及晴朗的天空吸收和散射（ra_sw_physics = 1）。
b.	戈达德短波：具有气候和云影响的臭氧的两流多频带方案（2）。
c.	GFDL短波：Eta操作方案。来自气候和云影响的具有臭氧的两流多频带方案（99）。
d.	CAM方案：来自CCSM中使用的CAM 3气候模型。允许浮质和微量气体（3）。
e.	RRTMG短波。采用MCICA方法的随机云重叠的新短波方案（4）。3.1版的新功能。对于主要痕量气体，CO 2 = 379e-6（2005年有效），N 2 O = 319e-9，CH 4 = 1774e-9。有关时变选项，请参见第2.3节。在V3.7中，作为选项24引入了快速版本。从V4.2开始，CO 2值被以下年份的函数所代替：CO2（ppm）= 280 + 90 exp（0.02 *（2000年））与观测值相比，在1920年代和1960年代误差约为4％，在2000年之后误差约为1％。
f.	戈达德计划（5）。来自简单气候的高效，多波段臭氧。设计用于与Goddard微物理学粒子半径信息一起运行。在V4.1中更新。
g.	傅留谷方案（7）。多波段，云层和云层的影响，气候学中的臭氧分布都可以形成气溶胶。3.4版的新功能。
h.	举行苏亚雷斯放松。仅设计用于理想化测试的温度松弛方案（31）。
一世。RRTMG-K（14）：Baek（2017）改进了RRTMG方案的版本。V4.0中的新功能。

相关选项：
-坡度和阴影效果。lope_rad = 1根据地形坡度修改表面太阳辐射通量。topo_shading = 1允许相邻网格单元的阴影化。仅用于网格尺寸小于几公里的高分辨率运行。从3.2版开始，这些功能可用于所有短波选件。
- swrad_scat ：散射转动参数为ra_sw_physics = 1。默认值是1，这相当于1.E-5米2 /公斤。当该值大于1时，会增加散射。
- swint_opt = 1 基于SW调用之间的更新太阳天顶角插值的短波辐射的：。自V3.5.1起可用。
 
- swint_opt = 2 激活快速全天空辐射模型的太阳能应用（场）。FARMS是一种快速的辐射传递模型，允许每个模型时间步长模拟宽带太阳辐射。该模型通过改变云的光学厚度，云的颗粒大小和太阳天顶角来使用云的透射率和反射率的查找表。Xie等人提供了更详细的描述。（2016）。
 
2.3辐射选项的输入
a.	CAM温室气体：每年提供1765年至2500年的温室气体。通过使用configure.wrf中添加的宏–DCLWRFGHG编译WRF，可以激活该选项。编译后，CAM，RRTM和RRTMG长波方案将发现这些气体。提供了五个方案文件：从IPCC AR5：CAMtr_volume_mixing_ratio .RCP4.5，CAMtr_volume_mixing_ratio.RCP6和CAMtr_volume_mixing_ratio.RCP8.5；来自IPCC AR4：CAMtr_volume_mixing_ratio.A1B和CAMtr_volume_mixing_ratio.A2。默认指向RCP8.5文件。3.5版的新功能。
b.	RRTMG的气候臭氧和气溶胶数据：臭氧数据改编自CAM辐射（ra _ * _ physics = 3），具有纬度（2.82度），高度和时间（每月）变化，这与RTMMG中使用的默认臭氧相反仅随高度变化的方案。这由名称列表选项o3input = 2 激活，这成为V3.7中的默认选项。气溶胶数据基于Tegen等。（1997年），它有6种类型：有机碳，黑碳，硫酸盐，海盐，粉尘和平流层气溶胶（火山灰，为零）。数据还具有空间（经度5度，纬度4度）和时间（每月）变化。该选项由名称列表选项aer_opt = 1 激活。版本3.5中的新增功能。
c.	RRTMG和Goddard辐射选项的气溶胶输入（aer_opt = 2 ）。AOD或AOD加Angstrom指数，单散射反照率和云非对称性参数都可以通过名称列表中的常量值或通过辅助输入流15的2D输入字段来提供。也可以设置气溶胶类型。V3.6中的新功能。
d.	RRTMG辐射方案的气溶胶输入来自气候友好的水和冰气溶胶（aer_opt = 3 ）。它与Thompson microphysics选项28一起使用。V3.8中的新增功能。
e.	RRTMG使用Thompson（自3.5.1起），WSM，WDM和NSSL微物理学方案（V3.7中的新功能），P3（自3.9）以来的有效云水，冰和雪半径。
 
2.4云分数选项
icloud ：= 1，使用Xu-Randall方法; = 2，使用阈值方法得出0或1的云分数；= 3，使用遵循Sundqvist等人的基于RH的方法。（1989）。RH的阈值取决于网格大小（V3.7中的新增功能，V3.8中的固定功能，在V3.9中进行了进一步调整）。
 
 
3.1表面层（sf_sfclay_physics）
a.	MM5相似度：基于具有Carslon-Boland粘性子层的Monin-Obukhov和查找表中的标准相似度函数（sf_sfclay_physics = 91）。在V3.7中，将海洋上的热和湿气粗糙度长度（或热和湿气的交换系数）更改为COARE 3公式（Fairall等，2003）。
b.	埃塔相似度：用于埃塔模型。基于具有Zilitinkevich的Monin-Obukhov的热粗糙度长度和查找表中的标准相似性函数（2）。
c.	Pleim-Xiu表面层。（7）。3.0版中的新功能。
d.	QNSE表面层。准标准比例消除PBL方案的表面层选项（4）。3.1版的新功能。
e.	MYNN表面层。Nakanishi和Niino PBL的表面层方案（5）。3.1版的新功能。
f.	TEMF表面层。总能量–质量通量表面层方案。版本3.3中的新增功能。
g.	修订的MM5表面层方案（V3.6之前的选项11，从V3.6起更名为选项1）：删除限制并使用更新的稳定性函数。3.4版的新功能。（Jimenez等，MWR 2012）。在V3.7中，代码加速以提供与旧MM5方案相似的时序。海洋中的热和湿气粗糙度长度（或热和湿气的交换系数）在V3.7中更改为COARE 3公式（Fairall等，2003）。
h.	其他：iz0tlnd = 1（与sf_sfclay_physics = 1、91 和5一起使用），Chen-Zhang在陆地上的热粗糙度长度，取决于植被高度，0 =每个sfclay选项中的原始热粗糙度长度。3.2版中的新功能。
 
 
3.2陆地表面（sf_surface_physics）
a.	（1）5层热扩散：仅土壤温度方案，使用5层。
b.	（2）诺亚陆地表面模型：统一的NCEP / NCAR / AFWA方案，具有四层土壤温度和湿度，分数积雪和冻结土壤物理特性。版本3.1中添加了新的修改，以更好地表示冰原和积雪区域的过程。
-在V3.6，子平铺选项被引入，并且它是由名称列表激活sf_surface_mosaic = 1，和瓷砖的在网格框的数目由名称列表定义mosaic_cat ，具有3的默认值。
c.（3）RUC地表模型：此模型采用分层方法求解能量和水分预算，因为在第一大气层和顶层土壤层的中间计算了大气通量以及土壤通量，这些通量分别改变了覆盖地面的层中的热量和水分。RUC LSM目前在与大气接触面附近的土壤中使用9个级别的高分辨率。（注意：如果从靠近地面的低分辨率模型（如Noah LSM）进行初始化，则最高水位可能太潮湿，从而导致模型预测中出现湿/冷偏差。解决方案：循环土壤水分，使其在30°C时旋转至少需要几天才能适应RUC LSM的垂直结构）。 
 
土壤水分的预后变量是体积土壤水分含量减去与土壤颗粒结合的剩余土壤水分，因此不参与水分传输。RUC LSM考虑了土壤中的冻结和解冻过程。它能够使用云微物理学方案提供的显式混合相降水。它具有简单的海冰处理方法，可以解决海冰中的热量扩散问题，并可以在海冰上形成积雪。在温暖的季节，RUC LSM修正农田区域的土壤水分，以补偿这些地区的灌溉。 
 
根据积雪深度，积聚在土壤顶部的积雪最多可以有两层（参考S16）。当雪层很薄时，它与表层土壤结合在一起，以避免夜间过度辐射冷却。当雪水当量低于阈值3 cm时，网格单元可能会被雪部分覆盖。当发生这种情况时，表面参数（例如粗糙度长度和反照率）将被计算为积雪和无雪区域的加权平均值。能源预算利用迭代积雪算法。融化的水可以部分冻结，并保留在雪层中，其余部分则渗入积雪，渗入土壤并形成地表径流。积雪密度随积雪温度，积雪深度和压实参数而变化。对于给定的植被类型，从最大雪反照率初始化雪反照率，但也可以根据雪温度和雪分数来修改雪反照率。为了更好地表示积雪在地面上的积雪，RUC LSM引入了冻结降水密度的估算。 
 
对RUC LSM的最新修改包括对机盖截留液体或冻结降水的改进，以及对零星积雪的“马赛克”方法，对雪域的积雪和无雪部分分别进行了能量和水分预算的处理网格单元，并在时间步长结束时聚合单独的解决方案。
 
初始化RUC LSM所需的数据集包括： 
1. 土壤和土地利用类型的高分辨率数据集；  
2. 无雪地区的气候反照率； 
3. 在有积雪的情况下最大表面反照率的空间分布； 
4. 网格单元中植被类型的分数，在计算表面参数时要考虑亚网格尺度的异质性；        
5. 网格单元内土壤类型的分数； 
6. 气候绿色度； 
7. 气候学叶面积指数； 
8. 土壤底部的气候平均温度； 
9. 实时海冰浓缩； 
10.实时积雪纠正RAP和HRRR雪场中循环的积雪。
 
推荐的名称列表选项：
 sf_surface_physics = 3 
 num_soil_layers    = 9 
 usemonalb    = .true。，       
 rdlai2d = .true。，           
 mosaic_lu = 1         
 mosaic_soil = 1       
 
参考文献：
Smirnova et al（2016，Mon.Wea.Rev。，S16）;
使用RUC LSM作为土地组成部分的RAP和HRRR：https ://rapidrefresh.noaa.gov/RAP 和https://rapidrefresh.noaa.gov/hrrr/HRRR。
 
（来自GSD / NOAA的Tanya Smirnova）
 
d.（7）Pleim-Xiu地表模型。有关PX LSM的详细说明（包括优点/缺点，最佳实践和最新改进），请参阅http://www2.mmm.ucar.edu/wrf/users/docs/PX-ACM.pdf
e.带植被和次网格平铺的两层方案（7）。3.0版中的新功能：多年来开发并改进了Pleim-Xiu地表模型（PX LSM； Pleim和Xiu 1995； Xiu和Pleim 2001），以提供真实的地面温度，土壤湿度以及地表敏感和潜热通量。中尺度气象模型。PX LSM基于ISBA模型（Noilhan和Planton 1989），并包括一个2层力恢复土壤温度和湿度模型。顶层的厚度为1厘米，下层的厚度为99厘米。网格集合植被和土壤参数来自土地利用类别和土壤质地类型的部分覆盖。通过动态调节土壤湿度（Pleim和Xiu，2003年）和深层土壤温度（Pleim和Gilliam，2009年），有两种间接的微调方案可以纠正2米空气温度和湿度的偏差。
用户应认识到，PX LSM主要是为追溯模拟而开发的，在该模拟中，基于表面的观测可为间接土壤钉结提供信息。尽管可以使用FDDA namelist.input设置“ pxlsm_soil_nudge”禁用土壤微动，但是在此模式下几乎未进行测试，尽管一些用户报告了合理的结果。Gilliam和Pleim（2010）讨论了WRF模型中的实现，并提供了用于回顾性应用的典型配置。如果激活了土壤打结，建模者必须使用Obsgrid物镜重新分析实用程序来生成具有命名约定“ wrfsfdda_d0 *”的表面打结文件。Obsgrid将获取WPS“ met_em *”文件和LittleR观察文件，并生成“ wrfsfdda_d0 *”文件。PX LSM使用2-m的温度，并根据该文件重新分析混合比，以获取深层土壤水分和温度调节效果。如果建模人员希望在预报模式下激活土壤微处理来测试PX LSM，则可以使用Obsgrid将预报的2-m温度和混合比与空的观测文件一起使用以生成“ wrfsfdda_d0 *”文件，但结果将与控制相关预测模型。
f. （4）Noah-MP（多物理场）陆面模型：在关键的陆-气相互作用过程中使用了多个选项。Noah-MP包含一个单独的植被树冠，该树冠由树冠顶部和底部定义，具有在包括遮蔽效应的两股冠层辐射传输方案中使用的叶片物理和辐射特性。Noah-MP包含一个具有液态水储存和融化/再冻能力的多层雪包，以及一个截雪模型，该模型描述了受顶篷拦截的雪的加载/卸载，融化/再冻和升华。地表水的渗透和径流以及地下水的转移和存储（包括地下水位到无限制含水层的深度）有多种选择。可以使用预测性光合作用和动态植被模型来规定或预测水平和垂直植被密度，该模型将碳分配给植被（叶，茎，木材和根）和土壤碳库（快速和慢速）。3.4版的新功能。（Niu et al.2011）
G。（8）SSiB地表模型：这是简化的简单生物圈模型的第三代（Xue等，1991； Sun和Xue，2001）。SSiB是为在气候模型中进行土地/大气相互作用研究而开发的。SSiB中的空气动力学阻力值是根据修改后的Monin-Obukhov相似性理论根据植被性质，地面条件和整体Richardson数确定的。SSiB-3包括三个降雪层，可以真实地模拟降雪过程，包括破坏性变质，由于降雪引起的致密化过程以及融雪，这大大增强了该模型在寒冷季节研究的能力。要使用此选项，应将ra_lw_physics 和ra_sw_physics 设置为1、3或4。第二个完整模型级别应设置为不大于0.982，以便该级别的高度高于植被高度。3.4版的新功能。
H。分数海冰（fractional_seaice = 1）。将海冰视为分数场。需要分数海冰作为输入数据。数据来源可能包括来自GFS或国家冰雪数据中心（http://nsidc.org/data/seaice/index.html）的数据。使用XICE而不是SEAICE来输入Vtable。在当前版本中，此选项适用于sf_sfclay_physics = 1、2、5 和7，而sf_surface_physics = 2、3和7。3.1版的新功能。
I。（5）CLM4（社区土地模型第4版，Oleson等，2010； Lawrence等，2010）：CLM4是在国家大气研究中心与许多外部合作者共同开发的，代表了科学发展的地表过程模型。它包含生物地球物理学，水文学，生物地球化学和动态植被的精密处理。在CLM4中，每个模型网格单元中的土地表面都被表征为五种主要的子网格土地覆盖类型（冰川，湖泊，湿地，城市和植被）。植被亚网格由多达4 种生理功能和结构不同的植物功能类型（PFT）组成。WRF输入土地覆盖类型通过查找表转换为CLM4 PFT。CLM4垂直结构包括一个单层植被冠层，一个五层积雪和一个十层土柱。在WRF中，Jin和Wen（2012; JGR-Atmosphere），Lu and Kueppers（2012; JGR-Atmosphere）和Subin等人在WRF中对CLM的早期版本进行了定量评估。（2011; Earth Interactions）（来自Jin ）。3.5版的新功能。在V3.6中针对20/21类别的MODIS土地利用数据进行了更新。 
 
 
3.3城市表面（sf_urban_physics –替换旧的开关ucmcall）
自V3.1起，orban物理选项就可与Noah LSM一起使用，而自V3.9起，则与NoahMP一起使用。
a.	城市雨棚模型（1）：3类UCM选项，具有针对屋顶，墙壁和街道的表面效果。在V3.7中，添加了绿色屋顶选项。
b.	BEP（2）。建筑环境参数化：多层城市雨棚模型，允许高于最低模型级别的建筑物。仅适用于Noah LSM和Boulac和MYJ PBL选项。3.1版的新功
c.	BEM（3）。建筑节能模型。增加了BEP，通过供暖和制冷系统实现建筑能耗预算。与BEP的选项相同。3.2版中的新功能。
 
3.4湖泊物理学（sf_lake_physics）
CLM 4.5湖泊模型（1）。湖泊方案是从社区土地模型版本4.5（Oleson等人，2013）获得的，Gu等人对此做了一些修改。（2013）。它是一维的质量和能量平衡方案，具有20-25个模型层，其中包括湖冰上最多5个雪层，湖底最多10个水层和10个土壤层。湖泊方案与WPS派生的实际湖泊点和湖泊深度一起使用，也可以与WRF中用户定义的湖泊点和湖泊深度一起使用（lake_min_elev和lakedepth_default）。湖泊方案独立于地表方案，因此可以与WRF中嵌入的任何地表方案一起使用。湖泊计划的发展和评估包括在Subin等人中。（2012）和Gu等。（2013）（Subin等，2012：气候模拟的改进湖泊模型，J。Adv。Model。Earth Syst。，4，M02001。DOI：10.1029 / 2011MS000072； Gu等，2013：湖泊表面温度的校准和验证模拟与耦合WRF湖模型。气候变化，1-13，10.1007 / s10584-013-0978-Y）。       
 
4.行星边界层（bl_pbl_physics）
a.	延世大学方案：在不稳定的混合层中具有显式夹杂层和抛物线K轮廓的非局部K方案（bl_pbl_physics = 1）。
- topo_wind ：= 1：表面地形校正卷绕到表示在山顶部（门尼斯和Dudhia，JAMC 2012）从分格地形额外的阻力和增强的流动。仅适用于YSU PBL。3.4版的新功能。= 2：更简单的与地形变化有关的校正。3.5版的新功能。
- ysu_topdown_pblmix ：= 1：自上而下选项混合由辐射冷却驱动。V3.7中的新功能。
b.	Mellor-Yamada-Janjic方案：Eta操作方案。一维局部湍流预混湍动能方案（2）。
c.	MRF方案：（a）的较旧版本，将夹带层作为非局部K混合层的一部分进行了隐式处理（99）。
d.	ACM2 PBL：具有非局部向上混合和局部向下混合的非对称对流模型（7）。3.0版中的新功能。
e.	准标准比例消除PBL（4）。一种TKE预测选项，它对稳定的分层区域使用新的理论（自3.1开始可用）。白天部分使用版本3.4中添加的具有浅对流（mfshconv = 1）的涡流扩散质量通量方法。
f.	Mellor-Yamada Nakanishi和Niino Level 2.5 PBL（5）。预测子网格TKE术语。3.1版中的新增功能，V3.8中进行了重大更新。
-icloud_bl：= 1，可以选择将亚网格规模的云从MYNN耦合到辐射；-bl_mynn_cloudpdf：= 1，Kuwano等人（2010）; = 2，Chaboureau和Bechtold（2002，JAS，带有mods，默认）; 
-bl_mynn_cloudmix：= 1，混合云水和冰（当scalar_pblmix = 1时，qnc和qni混合）；
g.	以上三个选项是V3.8中的新增功能。
-bl_mynn_edmf = 1，激活MYNN中的质量通量（从v3.9开始可以尝试）；
-bl_mynn_mixlength = 2：1来自RAP / HRRR，2来自混合（也可从v3.9获得）。
h.	Mellor-Yamada Nakanishi和Niino Level 3 PBL（6）。预测TKE和其他第二时刻。3.1版的新功能。
i.	Mellor-Yamada Nakanishi和Niino Level 3 PBL（6）。预测TKE和其他第二时刻。3.1版的新功能。
j.	BouLac PBL（8）：Bougeault-LacarrèrePBL。TKE预测选项。3.1版的新功能。设计用于BEP城市模型。
k.	UW（Bretherton and Park）计划（9）。来自CESM气候模型的TKE方案。版本3.3中的新增功能。
l.	总能量-质量通量（TEMF）方案（10）。子电网总能量预测变量，加上质量通量型浅对流。版本3.3中的新增功能。
m.	LES PBL：版本3中提供了大涡模拟（LES）边界层。为此，选择bl_pbl_physic = 0，isfflx = 1以及sf_sfclay_physics 和sf_surface_physics 。这使用扩散进行垂直混合，并且必须使用diff_opt = 2和km_opt = 2或3，请参见下文。使用isfflx = 0或2 选择运行LESPBL的其他理想方式。3.0 版中的新增功能。
n.	Grenier-Bretherton-McCaa方案（12）：这是一个TKE方案。经过云计算的PBL案例测试。3.5版的新功能。
o.	新洪方案（11）：在对流PBL中包括垂直传输的比例依赖性。YSU遵循在稳定的PBL和自由气氛中的垂直混合。该方案还诊断了TKE和混合长度输出。V3.7中的新功能。
p.	SMS-3DTKE：这是3D TKE子网格混合方案，可自适应大涡流模拟（LES）和中尺度极限之间的网格大小。可以通过设置bl_pbl_physic = 0，km_opt = 5，diff_opt = 2 来激活它，并且只能与sf_sfclay_physics = 1，5，91 一起使用。4.2版的新功能。

5.积云参数化（cu_physics）
a.	Kain-Fritsch方案：深质量流和浅层对流子网格方案，该方案使用质量流量法并具有下降气流和CAPE去除时间尺度（cu_physics = 1）。
-  kfeta_trigger = 1 -缺省触发器; = 2 –湿度对流调制触发函数基于Ma和Tan（2009，大气研究）。当大规模强迫作用较弱时，可能会改善亚热带地区的结果。
- cu_rad_feedback =真-允许与辐射分格云量相互作用。V3.6中的新功能。（Alapaty等人，2012年，《地球物理研究快报》）
b.	贝茨-米勒-詹吉克计划。运营Eta计划。色谱柱湿润度调节方案朝着混合均匀的轮廓（2）方向放松。
c.	Grell-Devenyi（GD）集成方案：多封闭，多参数，集成方法，通常具有144个子网格成员（在V3.5中移至选项93）。
d.	简化的Arakawa-Schubert（SAS）（4）。简单的质量通量方案，带有准平衡封闭，浅混合方案（仅在NMM中具有动量传递）。适用于版本3.3中的ARW。
e.	Grell 3D是GD方案的改进版本，如果打开了沉降扩展（选项cugd_avedx），它也可以用于高分辨率（除了较粗的分辨率）（5）。3.0版中的新功能。
f.	Tiedtke方案（美国夏威夷版）（6）。具有CAPE去除时间尺度，浅层分量和动量传递的质量通量类型方案。版本3.3中的新增功能。
g.	Zhang-McFarlane方案（7）。来自CESM气候模型的带动量传递的质量通量CAPE去除型深对流。版本3.3中的新增功能。
h.	新的简化的Arakawa-Schubert（NSAS）（96）。具有深浅成分和动量传递的新质量通量方案。版本3.3中的新增功能。这是V3。*中的选项14。
i.	新的简化的Arakawa-Schubert（84，HWRF版本）。具有深浅成分和动量传递的新质量通量方案。3.4版的新功能。
j.	Grell-Freitas（GF）方案（3）：一种改进的GD方案，试图平滑过渡到解决云的规模，这是Arakawa等人提出的。（2004）。3.5版的新功能。
k.	旧的Kain-Fritsch方案：使用对流和CAPE去除时间尺度的质量通量方法的深对流方案（99）。
l.	多尺度Kain-Fritsch方案（11）：使用基于尺度的动态调整时标，基于LCC的夹带。还使用基于Bechtold的新触发功能。V3.7中的新功能。V4.0中添加了使用CESM气雾剂的选项。在V4.2中，增加了对流动量传输。可以通过设置cmt_opt_flag = .false 来关闭它。里面的代码。
m.	新的Tiedtke方案（16）：此版本类似于REGCM4和ECMWF cy40r1中使用的Tiedtke方案。V3.7中的新增功能，V3.8中已更新。
n.	Kain-Fritsch-Cumulus势能方案（10）：此选项使用概率密度函数（PDFs）使用累积势能方案通过与边界层湍流相关联的一个来修改KF ad-hoc触发函数。该方案还根据与浅积云有关的时间尺度来计算积云分数。（Berg et al.2013。）V3.8中的新增功能。
o.	KIAPS SAS（14）：基于NSAS，但可感知规模。V4.0中的新功能。

6.浅对流选项（shcu_physics）
a.	.ishallow = 1，启用浅对流选项。与Grell 3D方案一起使用（cu_physics = 5）–将来将移至shcu_physics类别。
b.	UW（布雷瑟顿和帕克）计划（2）。CESM气候模型中带动量传递的浅积云选择。版本3.3中的新增功能。
c.	GRIMS（全球/区域集成建模系统）方案（3）：它使用涡流扩散和pal算法表示浅对流过程，并直接与YSU PBL方案耦合。3.5版的新功能。
d.	.NSAS浅层方案（4）：这是从NSAS中提取的，应与KSAS深层积层方案一起使用。V4.0中的新功能。
e.	.邓浅方案（5）：仅与MYNN和MYJ PBL方案一起运行。V4.1中的新功能。

7.其他物理选项
a.	用于热带风暴和飓风应用的选项：
-sf_ocean_physics = 1（在以前的版本中从omlcall 重命名）：简单的海洋混合层模型（1）：遵循Pollard，Rhines和Thompson（1972）的一维海洋混合层模型。其他两个名称列表选项可用于指定初始混合层深度（尽管可能会摄取实际的混合层深度数据）（oml_hml0 ）和低于混合层的温度降低速率（oml_gamma ）。从V3.2开始，此选项可用于所有sf_surface_physics 选项。
-sf_ocean_physics = 2：V3.5中的新功能。基于Price等人的3D Price-Weller-Pinkel（PWP）海洋模型。（1994）。该模型预测水平对流，压力梯度力以及混合层过程。在V3.5中，仅通过名称列表变量ocean_z，ocean_t 和ocean_s的简单初始化可用。
-isftcflx ：修改表面体阻力（Donelan）和焓系数，使其与热带风暴和飓风的最新研究结果更加一致。此选项还包括热通量中的耗散加热项。这是仅适用于sf_sfclay_physics = 1，有用于计算焓系数的两个选项：isftcflx = 1：常数ž 0Q （因为V3.2），用于热和水分; isftcflx = 2 Garratt配方，热量和水分的形式略有不同。
b.	长时间模拟的其他选项（版本3.1中的新增功能）：
- tmn_update ：更新深层土壤温度（1）。
- sst_skin ：根据曾和Beljaars（2005）计算的皮肤SST（1）
-bucket_mm ：水当量降水累积的桶重置值（以mm为单位的值，-1 =无效）。
-bucket_J ：能量累积的存储桶重置值（以焦耳为单位的值，-1 =无效）。仅适用于CAM和RRTMG辐射（ra_lw_physics = 3和4以及ra_sw_physics = 3和4）选项。
-要使用没有leap年的气候数据驱动WRF模型，有一个编译选项可以做到。编辑configure.wrf 并添加-DNO_LEAP_CALENDAR给宏ARCH_LOCAL。
c.	土地模型输入选项：
-usemonalb ：设置为.true时，它使用来自geogrid的每月反照率字段，而不是表值
- rdlai2d ：当设置为.TRUE，它使用从格栅月LAI数据（新的V3.6）和现场还将前往wrflowinp文件，如果sst_update 为1。
d.	gwd_opt ：重力波拖动选项。建议用于所有网格尺寸。该方案包括两个子网格的地形效果：重力波阻力和低水平流阻。后者是在V3.7中添加的。从V4.0开始，方案的输入风将旋转到地球坐标，然后将输出调整回投影域。这使该方案可以用于WRF支持的所有地图投影。为了正确应用此选项，必须使用来自geogrid的适当输入字段。有关详细信息，请参见本指南第3章中的“为重力波拖曳方案选择静态数据”部分。版本3.1中的新增功能，已在V3.7和V4.0中更新
e.	windfarm_opt ：风力涡轮机阻力参数化方案。它表示特定涡轮机在风场和TKE场上的子电网效应。从文件中读取风电场的物理特性，并建议使用制造商的规范。run / wind-turbine-1.tbl中提供了该文件的示例。涡轮机的位置从文件windturbines.txt中读取。有关更多详细信息，请参见WRF /目录中的README.windturbine。版本3.3中的新增功能，在此版本中，它仅适用于2.5级MYNN PBL选项（bl_pbl_physics = 5），并在V3.6中进行了更新。
f.	地表灌溉参数化：V4.2中增加了三种灌溉方案，可在模型中表示地表灌溉过程，并明确控制水量和灌溉时间（有关更多信息，请参阅https://doi.org/10.5194/gmd-2019- 233）。该方案（物理名称列表）代表不同的技术，具体取决于应用过程中的水蒸发损失。蒸发过程考虑以下因素造成的损失：
       sf_surf_irr_scheme = 1：表面蒸散（仅适用于Noah-LSM）
       sf_surf_irr_scheme = 1：树叶/冠层截留和表面蒸散
       sf_surf_irr_scheme = 3：微观物理过程，叶片/冠层截留和表面蒸散
每天使用的灌溉水量定义为“ irr_daily_amount ”（毫米/天）。该应用程序在一天中的时间段从“ irr_start_hours ” UTC值开始，持续“ irr_num_hours”。”一年中的灌溉时间范围由“ irr_start_julianday ”和“ irr_end_julianday” 内的儒略日定义。要考虑大于每天的灌溉间隔，可以将“ irr_freq ”设置为大于1的值。因此，在“ irr_freq ”期间内的活动日中应用的水量为（irr_daily_amount * irr_freq ）。“ irr_ph ”调节灌溉的空间激活（irr_freq > 1），尤其是确定是否在同一天为所有域激活了灌溉（irr_ph = 0）。对于irr_ph 不等于0 给出了两个选项：
          irr_ph = 1：激活场是（i，j，IRRIGATION）的函数
          irr_ph = 1：使用fortran RANDOM函数创建激活字段
      考虑到WRF中可能存在多个巢，对于每个模拟，灌溉方案应仅在一个域上运行。这样可以确保不重复使用水，并且与计算的irr_daily_amount 保持一致。有关代码更改的更多信息，请参见https://github.com/wrf-model/WRF/commit/ 9 bd5b61d9a。

两个域案例的灌溉名称列表参数示例：
  sf_surf_irr_scheme = 0、1 
  irr_daily_amount = 0,8                     
  irr_start_hour = 0，14                         
  irr_num_hours = 0，2                         
  irr_start_julianday = 0，121                   
  irr_end_julianday = 0，170     
  irr_ph = 0，0                       
  irr_freq = 0，3                      
这些设置将使用通道方法从14 UTC开始以2毫米/天的值灌溉内部区域2个小时。灌溉从儒略日121开始，到儒略日170结束。每3天将水同时浇灌到所有灌溉网格点的整个内部区域（irr_freq = 3）。这导致每小时灌溉12毫米/小时（每天应用24毫米），然后乘以网格单元内的灌溉百分比（由WPS中处理的灌溉字段决定）。
8.物理灵敏度选项
a.	no_mp_heating ：设置为1时，它将关闭微物理学的潜热。使用此选项时，应将cu_physics 设置为0 
b.	icloud ：设置为0时，它将在短波辐射选项1、4和长波辐射选项1、4中关闭对光学深度的云影响。请注意，自V3.6起，此名称列表还控制使用哪种云分方法进行辐射。
c.	isfflx ：设置为0时，它将关闭表面的显热通量和潜热通量。此选项适用于sf_sfclay_physics = 1、5、7、11。d 
d.	ifsnow ：设置为0时，它将关闭sf_surface_physics = 1中的积雪效果。

扩散和阻尼选项Diffusion and Damping Options
WRF中的扩散分为两个参数：扩散选项和K选项。扩散选项选择如何计算扩散中使用的导数，而K选项选择如何计算K系数。请注意，当选择PBL选项时，垂直扩散是通过PBL方案而不是扩散方案完成的。在版本3中，垂直扩散也与表面通量有关。
1.1扩散选项（diff_opt）
a.	简单扩散：简单地沿坐标曲面获取渐变（diff_opt = 1）。
b.	完全扩散：梯度使用完全度量术语来更精确地计算倾斜坐标中的水平梯度（diff_opt = 2）。从V3.6.1开始，此选项可用于实际数据案例。
1.2 K选项（km_opt）
请注意，使用PBL方案时，只有以下选项（a）和（d）才有意义，因为（b）和（c）设计用于3d扩散。
a.	常数：K由水平和垂直扩散的名称列表值指定（km_opt = 1）。
b.	3d TKE：使用了湍流动能的预后方程，并且K基于TKE（km_opt = 2）。
c.	3d变形：根据Smagorinsky方法（km_opt = 3）通过3d变形和稳定性来诊断K。
d.	2d变形：仅通过水平变形即可诊断出水平扩散的K。假定垂直扩散是通过PBL方案完成的（km_opt = 4）。 
1.3 六阶水平扩散（diff_6th_opt）
6六阶水平超上的所有变量扩散（del^ 6）以充当选择性短波数值噪声滤波器。可以与diff_opt 结合使用。diff_6th_opt = 1：simple；= 2：positive definite。默认值为0（off），但是如果需要使用它，则建议使用选项2（应避免使用选项1）。在V4.0中，引入了一些控件：diff_6th_slopeopt （0,1）控制是否在陡峭的地形上关闭此选项；diff_6th_thresh 设置地形坡度的阈值，高于该阈值将关闭此选项。参见Knievel等。了解有关此选项的更多信息。
1.4非线性反向散射各向异性（NBA）（sfs_opt）
用于LES应用中动量的亚网格湍流应力选项。3.2版中的新功能。sfs_opt = 1诊断分格应力与使用diff_opt = 2和km_opt = 2或3。sfs_opt =与其一起使用TKE分格应力diff_opt = 2和km_opt = 2。 
 
2.阻尼选项
这些是独立激活的选择。
a.	上部阻尼：可以在模型顶部附近添加增加扩散的层（wet_opt = 1）或瑞利弛豫层（2）或隐式重力波阻尼层（3，版本3.0中的新增层），以控制来自上限。
b.	垂直速度阻尼（w_damping ）：为了提高操作的鲁棒性，可以对垂直运动进行阻尼，以防止模型在局部较大的垂直速度下变得不稳定。这仅影响强上升气流核心，因此对结果的影响很小。
c.	发散阻尼（sm_div ）：控制水平传播的声波。
d.	外部模式阻尼（em_div ）：控制上表面（外部）波。
e.	偏心时间（epssm ）：控制垂直传播的声波。

平流选项
a.	动量（水平平流订单h_mom_adv_order ）和标量（h_sca_adv_order ）可以是2 次至6 次，用5 个顺序是所述推荐的一个。
b.	动量（垂直对流订单v_mom_adv_order ）和标量（v_sca_adv_order）可以是2 次和第6次，用3 次顺序为推荐的一个。
c.	单调传输（选项2，版本3.1中的新选项）和正定对流选项（选项1）可以应用于湿度（moist_adv_opt ），标量（scalar_adv_opt ），化学变量（chem_adv_opt ）和tke （tke_adv_opt ）。选项1替换pd_moist = .true。等等。
d.	WENO（基本上加权无振荡）（5选项3 次顺序WENO;选项4 5 次顺序WENO与正定限制器）：水分（moist_adv_opt ），标量（scalar_adv_opt ），化学变量（chem._adv_opt ）和TKE （tke_adv_opt ）。对于动量，动量_adv_opt = 3。
有关使用单调和正定对流选项的一些注意事项：
正定和单调选项可用于ARW解算器中的水分，标量，化学除垢剂和TKE。单调和正定运输选项都可以在本地和全局范围内保存标量质量，并且与ARW质量守恒方程式一致。我们建议在所有实际数据模拟中对湿度变量使用正定选项。单调选项在化学应用中以及某些情况下对于水分和标量可能是有益的。  
 
使用这些选项时，在仿真配置中应考虑ARW集成方案的某些方面。
1)	当使用正定或单调选项时，ARW中的积分顺序会更改。当未激活选项时，来自物理学（不包括微观物理学）的时步趋势将在传输（对流）的同时用于更新标量混合比。基于传输+物理更新，可以计算微物理，并更新水分。当激活单调或正定选项时，首先根据物理趋势更新标量混合比，然后将新的更新值用作传输方案的起始值。在使用这些最新值作为起点的传输更新之后，将进行微物理更新。重要的是要记住，对于任何标量，局部和全局守恒性质，正定性和单调性都取决于具有这些性质的每次更新。   
2)	某些模型过滤器可能不是正定的。
i.	diff_6th_opt = 1既不是正定也不单调。如果需要此扩散选项，请使用diff_6th_opt = 2（diff_6th_opt = 2是单调且正定的）。我们遇到了非常明显的单调性和正定性偏差的情况。  
ii.	由于可变的涡流扩散系数K，diff_opt = 1和km_opt = 4（常用的实数据案例混合选项）不能保证是正定的或单调的。我们还没有观察到明显偏离正定性或单调性的情况。当此过滤器与这些运输选项一起使用时。 
iii.	使用用户指定的恒定涡流粘度的扩散选项为正定和单调的。
iv.	使用可变涡流粘度的其他过滤器选项不是正定的或单调的。
3)	大多数模型物理学不是单调的也不应该是单调的-它们表示系统中的源和汇。尽管我们尚未检查和测试此属性的所有选项，但所有这些都应为肯定的。
4)	单调选项为活动区域的传输增加了极大的平滑度。您可能需要考虑使用单调传输为变量关闭其他模型过滤器（例如二阶和六阶水平过滤器）。目前，无法使用名称列表关闭标量的过滤器，而无法关闭动力学的过滤器-必须在求解器中手动注释掉调用。   

其他动力学选项
a.	通过将non_hydrostatic 开关设置为.false，可以静水运行模型。
b.	科里奥利术语只能应用于风扰动（pert_coriolis = .true。）（仅理想化）。
c.	仅对于diff_opt = 2，垂直扩散可能作用于整个场（不只是作用于一维基本轮廓的扰动（mix_full_fields = .true 。；仅理想化）。
d.	为了获得更准确的水分解决方案，可以添加
use_q_diabatic ：一步一步地考虑了微观物理学中的水分趋势。此选项可能会使时间步长更具限制性。
use_theta_m ：分步考虑水分对压力的影响。当前的实现可能会花费更多的运行时间。

横向边界条件选项
a.	周期性（periodic_x / periodic_y ）：用于理想情况。
b.	打开（open_xs，open_xe，open_ys，open_ye ）：用于理想情况。
c.	对称（symmetric_xs，symmetric_xe，symmetric_ys，symmetric_ye ）：用于理想情况。
d.	指定（指定）：用于实际数据案例。第一行和第一列由外部模型值指定（spec_zone = 1，并且不应更改）。Relax_zone中的行和列具有来自外部模型和WRF的混合值。只要spec_bdy_width = spec_zone + Relax_zone ，就可以更改Relax_zone 的值。可以在热带通道模拟中与periodic_x 一起使用。
spec_exp ：松弛带斜坡的指数乘数，用于指定的边界条件。0。=线性斜坡，默认值；0.33 =〜3 * dx exp衰减因子。这对于长时间的仿真可能很有用。

<a id=PBL_Physics></a>

### PBL物理选项摘要

bl_pbl_physics	Scheme	Reference	Added
1	YSU	Hong, Noh and Dudhia (2006, MWR)
2004
2	MYJ	Janjic (1994, MWR)
2000
3	GFS	Hong and Pan (1996, MWR)
2005
4	QNSE	Sukoriansky, Galperin and Perov (2005, BLM)
2009
5	MYNN2	Nakanishi and Niino (2006, BLM)
2009
6	MYNN3	Nakanishi and Niino (2006, BLM)
2009
7	ACM2	Pleim (2007, JAMC)
2008
8	BouLac	Bougeault and Lacarrere (1989, MWR)
2009
9	UW	Bretherton and Park (2009, JC)
2011
10	TEMF	Angevine, Jiang and Mauriten (2010, MWR)
2011
11	Shin-Hong	Shin and Hong (2015, MWR)
2015
12	GBM	Grenier and Bretherton (2001, MWR)
2013
99	MRF	Hong and Pan (1996, MWR)
2000

bl_pbl_
physics
	Scheme
方案	Cores
	sf_sfclay_
physics
	Prognostic variables
预后变量	Diagnostic variables
诊断变量	Cloud mixing
云混合
1
	YSU
	ARW/ NMM
	1, (91)*
	
	exch_h
	QC,QI

2
	MYJ
	ARW/ NMM
	2
	TKE_PBL
	el_myj, exch_h
	QC,QI

3
	GFS
(hwrf)
	NMM
	3
	
	
	QC,QI

4
	QNSE-EDMF
	ARW/ NMM
	4
	TKE_PBL
	el_pbl, exch_h, exch_m	QC,QI

5
	MYNN2
	ARW
	1,2,5,(91)
	QKE
	Tsq, Qsq, Cov, exch_h, exch_m
	QC

6
	MYNN3
	ARW
	1,2,5,(91)
	QKE, Tsq, Qsq, Cov
	exch_h, exch_m
	QC

7
	ACM2
	ARW
	1,7,(91)
	
	
	QC,QI

8
	BouLac
	ARW
	1,2,(91)
	TKE_PBL
	el_pbl, exch_h, exch_m, wu_tur, wv_tur, wt_tur, wq_tur	QC

9	UW	ARW	1,2,(91)	TKE_PBL	exch_h, exch_m	QC
10	TEMF	ARW	10	TE_TEMF	*_temf	QC, QI
11	Shin-Hong	ARW	1,(91)		 exch_h, tke_diag	QC, QI
12	GBM	ARW	1,(91)	TKE_PBL	el_pbl, exch_tke
	QC, QI
99
	MRF
	ARW/ NMM
	1,(91)
	
	
	QC, QI

* 在版本3.6中，sfclay选项11重命名为1，原始选项1重命名为91。

<a id=Microphysics></a>

### 微物理学选项摘要

mp_physics	Scheme	Reference	Added

1	Kessler	Kessler (1969)
2000
2	Purdue Lin 	Chen and Sun (2002, JMSJ)
2000
3	WSM3	Hong, Dudhia and Chen (2004, MWR)
2004
4	WSM5	Hong, Dudhia and Chen (2004, MWR)
2004
5	Eta (Ferrier)	Rogers, Black, Ferrier, Lin, Parrish and DiMego (2001, web doc)	2000
6	WSM6	Hong and Lim (2006, JKMS)
2004
7	Goddard 4-ice	Tao, Simpson and McCumber (1989, MWR), and Tao et al. (2016, JGRA)
2019
8 	Thompson	Thompson, Field, Rasmussen and Hall (2008, MWR)https://doi.org/10.1175/2008MWR2387.1
2009
9	Milbrandt 2-mom	Milbrandt and Yau (2005, JAS, part I, part II)
2010
10	Morrison 2-mom	Morrison, Thompson and Tatarskii (2009, MWR)
2008
11	CAM 5.1	Neale et al. (2012, NCAR Tech Note)
2013
13	SBU-YLin	Lin and Colle (2011, MWR)
2011
14	WDM5	Lim and Hong (2010, MWR)
2009
16	WDM6	Lim and Hong (2010, MWR)
2009
17	NSSL 2-mom	Mansell, Ziegler and Bruning (2010, JAS)
2012
18	NSSL 2-mom w/
CCN prediction	Mansell, Ziegler and Bruning (2010, JAS)
2012
19	NSSL 1-mom		2013
21	NSSL 1-momlfo		2013
22	NSSL 2-mom w/o hail		2015
24	WSM7	Bae et al. (2018, APJAS)
2019
26	WDM7	Bae et al. (2018, APJAS)
2019
28	Thompson aerosol-aware	Thompson and Eidhammer (2014, JAS)
2014
30	HUJI SBM ‘fast’	Khain et al. (2010, JAS)
2014
32	HUJI SBM full	Khain et al. (2004, JAS)
2014
40	Morrison+CESM aerosol	EPA	2018
50/51/52	P3	Morrison and Milbrandt (2015, JAS)
2017
55	Jensen ISHMAEL	Jensen et al. (2017, JAS)
2019

mp_physics	Scheme	Cores	Mass Variables	Number Variables
1	Kessler	ARW	Qc Qr	

2	Purdue Lin	ARW (Chem)	Qc Qr Qi Qs Qg	

3	WSM3	ARW	Qc Qr	

4	WSM5	ARW/NMM	Qc Qr Qi Qs	

5	Eta (Ferrier)	ARW/NMM	Qc Qr Qs (Qt*)	

6	WSM6	ARW/NMM	Qc Qr Qi Qs Qg	

7	Goddard 4-ice	ARW/NMM	Qv Qc Qr Qi Qs Qg Qh	
8 	Thompson	ARW/NMM	Qc Qr Qi Qs Qg 	Ni Nr
9	Milbrandt 2-mom	ARW	Qc Qr Qi Qs Qg Qh	Nc Nr Ni Ns Ng Nh
10	Morrison 2-mom	ARW (Chem)	Qc Qr Qi Qs Qg	Nr Ni Ns Ng
11	CAM 5.1	ARW	Qc Qr Qi Qs Qg	Nr Ni Ns Ng
13	SBU-YLin	ARW	Qc Qr Qi Qs	
14	WDM5	ARW	Qc Qr Qi Qs	Nn** Nc Nr
16	WDM6	ARW	Qc Qr Qi Qs Qg 	Nn** Nc Nr
17	NSSL 2-mom	ARW	Qc Qr Qi Qs Qg Qh	Nc Nr Ni Ns Ng Nh
18	NSSL 2-mom
+CCN	ARW	Qc Qr Qi Qs Qg Qh	Nc Nr Ni Ns Ng Nh Nn Vg
19	NSSL 1-mom	ARW	Qc Qr Qi Qs Qg Qh	Vg***
21	NSSL 1-momlfo	ARW	Qc Qr Qi Qs Qg	
22	/nssl 2-mom	ARW	Qc Qr Qi Qs Qg 	Nc Nr Ni Ns Ng 
24	WSM6	ARW	Qv Qc Qr Qi Qs Qg Qh	
26	WDM7	ARW	Qv Qc Qr Qi Qs Qg Qh	
28	Thompson aerosol-aware	ARW/NMM	Qc Qr Qi Qs Qg	Ni Nr Nwf Nif
30	HUJI fast 	ARW	Qc Qr Qs Qg Qi	Nc Nr Ns Ni Ng Nn
32	HUJI full	ARW	Qc Qr Qs Qg Qh Qip Qic Qid Qnn	Nc Nr Ns Ng Nip Nic Nid Nn
40	Morrison with aerosol	ARW	Qc Qr Qi Qs Qg	Nr Ni Ns Ng
50	P3	ARW	Qc Qr Qi	Nr Ni Ri+ Bi++
51	P3-nc	ARW	Qc Qr Qi	Nc Nr Ni Ri Bi
52	P3-2ice	ARW	Qc Qr Qi,Qi2	Nc Nr Ni Ri Bi, Ni2, Ri2, Bi2
55	Jensen ISHMAEL	ARW	Qv Qc Qr Qi Qi2 Qi3	
* Advects only total condensates   ** Nn = CCN number  *** Vg: graupel volume
+ Rimed ice mass ++ rimed ice volume

<a id=Cumulus_Parameterization></a>

### 积云参数化选项摘要

cu_physics	Scheme	Reference	Added

1	Kain-Fritsch	Kain (2004, JAM)
2000
2	Betts-Miller-Janjic	Janjic (1994, MWR; 2000, JAS)
2002
3	Grell-Freitas	Grell and Freitas (2014, ACP)
2013
4	Old Simplied Arakawa-Schubert	Pan and Wu (1995), NMC Office Note 409
	2005/
2011
5	Grell-3	Grell (1993, MWR), Grell and Devenyi (2002, GRL)
2008
6	Tiedtke	Tiedtke (1989, MWR), Zhang et al. (2011, MWR)	2011
7	Zhang-McFarlane	Zhang and McFarlane (1995, AO)
2011
10	KF-CuP	Berg et al. (2013, MWR)
2016
11	Multi-scale KF	Zheng et al. (2016, MWR)
2015
14 	KIAPS SAS	Han and Pan (2011, Wea. Forecasting), Kwon and Hong (2017, WMR)
2018
16	New Tiedtke	Zhang and Wang (2017, JCLI)
2015
84	New SAS (HWRF)	Han and Pan (2011, Wea. Forecasting)
2012
93	Grell-Devenyi	Grell and Devenyi (2002, GRL)
2002
99	Old Kain-Fritsch	Kain and Fritsch (1990, JAS; 1993, Meteo. Monogr.)
2000

cu_physics	Scheme	Cores	Moisture Tendencies	Momentum Tendencies	Shallow Convection
1	Kain-Fritsch	ARW / NMM	Qc Qr Qi Qs	no
	yes
2	BMJ	ARW / NMM	-	no
	yes
3	GF	ARW	Qc Qi	no	yes
4	OSAS	ARW / NMM	Qc Qi	yes (NMM)	yes (ARW)
5	G3	ARW	Qc Qi	no	yes
6	Tiedtke	ARW / NMM	Qc Qi	yes	yes
7	Zhang-McFarlane	ARW	Qc Qi	yes 	no
10	KF-CuP	ARW	Qc Qr Qi Qs	no	yes
11	Multi-scale KF	ARW	Qc Qr Qi Qs	no	yes
96	NSAS	ARW	Qc Qr Qi Qs	yes	yes
14	KSAS	ARW	Qc Qr Qi Qs	yes	use shcu_physics=4
16	New Tiedtke	ARW	Qc Qi	yes	yes
84	NSAS (HWRF)	NMM	Qc Qi	yes	
93	GD	ARW	Qc Qi	no	no
99	old KF	ARW	Qc Qr Qi Qs	no	no

<a id=Radiation></a>

### 辐射物理选项摘要

ra_sw_physics	Scheme	Reference	Added
1	Dudhia	Dudhia (1989, JAS)
2000
3	CAM	Collins et al. (2004, NCAR Tech Note)
2006
4	RRTMG	Iacono et al. (2008, JGR)
2009
24	RRTMG	Fast version	2015
14	RRTMG-K	Baek (2017, JAMES)
2018
5	Goddard	Chou and Suarez (1999, NASA Tech Memo), Matsui et al. (2018, CD)
2011, updated 2019
7	FLG	Gu et al. (2011, JGR), Fu and Liou (1992, JAS)
2012
99	GFDL	Fels and Schwarzkopf (1975, JGR)
2004

ra_sw_
physics	Scheme	Cores+Chem	Microphysics Interaction	Cloud Fraction	Ozone
1	Dudhia	ARW NMM + Chem(PM2.5)	Qc Qr Qi Qs Qg	1/0	none
2	GSFC	ARW+Chem(τ)	Qc Qi	1/0	5 profiles
3	CAM	ARW	Qc Qi Qs	max-rand overlap	lat/month
4	RRTMG	ARW + Chem (τ), NMM	Qc Qr Qi Qs	max-rand overlap	1 profile or lat/month
5	Goddard	ARW	Qc Qr Qi Qs Qg	1/0	5 profiles
7	FLG	ARW	Qc Qr Qi Qs Qg	1/0	5 profiles
14	RRTMG-K	ARW	Qc Qr Qi Qs	max-rand overlap	1 profile or lat/month
24	RRTMG				
99	GFDL	ARW NMM	Qc Qr Qi Qs	max-rand overlap	lat/date

ra_lw_physics	Scheme	Reference	Added
1	RRTM	Mlawer et al. (1997, JGR)	2000
3	CAM	Collins et al. (2004, NCAR Tech Note)	2006
4	RRTMG	Iacono et al. (2008, JGR)	2009
24	RRTMG	Fast version	2015
14	RRTMG-K	Baek (2017)	2018
5	Goddard	Chou and Suarez (1999, NASA Tech Memo), Matsui et al. (2018, CD)
2011, updated 2019
7	FLG	Gu et al. (2011, JGR), Fu and Liou (1992, JAS)	2012
31	Held-Suarez		2008
99	GFDL	Fels and Schwarzkopf (1981, JGR)	2004

ra_lw_
physics	Scheme	Cores+Chem	Microphysics Interaction	Cloud Fraction	Ozone	GHG
1	RRTM	ARW NMM 	Qc Qr Qi Qs Qg	1/0	1 profile	constant or yearly GHG
3	CAM	ARW	Qc Qi Qs	max-rand overlap	lat/month	yearly CO2 or yearly GHG
4	RRTMG	ARW + Chem (τ), NMM	Qc Qr Qi Qs	max-rand overlap	1 profile or lat/month	yearly CO2 or yearly GHG
5	New Goddard	ARW	Qc Qr Qi Qs Qg	1/0	5 profiles	constant
7	FLG	ARW	Qc Qr Qi Qs Qg	1/0	5 profiles	constant
14	RRTMG-K	ARW	Qc Qr Qi Qs	max-rand overlap	1 profile or lat/month	constant
24	RRTMG					
31	Held-Suarez	ARW	none	none		none
99	GFDL	ARW NMM	Qc Qr Qi Qs	max-rand overlap	lat/date	constant


<a id=Namelist_Variables></a>

### namelist变量描述

在下面的namelist变量的说明。嵌套函数的变量由变量后的（max_dom）表示。另请参阅WRF /中的Registry / Registry.EM 和run / README.namelist 文件以获取更多详细信息。
可以参考别的资料，表超级长。


<a id=Output_Fields></a>

### WRF输出字段

fields清单
以下是netCDF命令'ncdump -h'的已编辑输出列表。请注意，有效的输出字段将取决于所使用的模型选项。如果字段具有零值，则不会通过所选的模型选项来计算字段。
ncdump -h wrfout_d<domain>_<date>
 
   netcdf wrfout_d01_2018-07-14_12:00:00

维度dimensions:
	Time = UNLIMITED ; // (1 currently)
	DateStrLen = 19 ;
	west_east = 500 ;
	south_north = 500 ;
	bottom_top = 55 ;
	bottom_top_stag = 56 ;
	soil_layers_stag = 4 ;
	west_east_stag = 501 ;
	south_north_stag = 501 ;

变量variables:
		float XLAT(Time, south_north, west_east) ;
		XLAT:description = "LATITUDE, SOUTH IS NEGATIVE" ;
		XLAT:units = "degree_north" ;
	float XLONG(Time, south_north, west_east) ;
		XLONG:description = "LONGITUDE, WEST IS NEGATIVE" ;
		XLONG:units = "degree_east" ;
	float LU_INDEX(Time, south_north, west_east) ;
		LU_INDEX:description = "LAND USE CATEGORY" ;
		LU_INDEX:units = "" ;
	float ZNU(Time, bottom_top) ;
		ZNU:description = "eta values on half (mass) levels" ;
		ZNU:units = "" ;
	float ZNW(Time, bottom_top_stag) ;
		ZNW:description = "eta values on full (w) levels" ;
		ZNW:units = "" ;
	float ZS(Time, soil_layers_stag) ;
		ZS:description = "DEPTHS OF CENTERS OF SOIL LAYERS" ;
		ZS:units = "m" ;
	float DZS(Time, soil_layers_stag) ;
		DZS:description = "THICKNESSES OF SOIL LAYERS" ;
		DZS:units = "m" ;
	float VAR_SSO(Time, south_north, west_east) ;
		VAR_SSO:description = "variance of subgrid-scale orography" ;
		VAR_SSO:units = "m2" ;
	float U(Time, bottom_top, south_north, west_east_stag) ;
		U:description = "x-wind component" ;
		U:units = "m s-1" ;
	float V(Time, bottom_top, south_north_stag, west_east) ;
		V:description = "y-wind component" ;
		V:units = "m s-1" ;
	float W(Time, bottom_top_stag, south_north, west_east) ;
		W:description = "z-wind component" ;
		W:units = "m s-1" ;
	float PH(Time, bottom_top_stag, south_north, west_east) ;
		PH:description = "perturbation geopotential" ;
		PH:units = "m2 s-2" ;
	float PHB(Time, bottom_top_stag, south_north, west_east) ;
		PHB:description = "base-state geopotential" ;
		PHB:units = "m2 s-2" ;
	float T(Time, bottom_top, south_north, west_east) ;
		T:description = "perturbation potential temperature theta-t0" ;
		T:units = "K" ;
	float THM(Time, bottom_top, south_north, west_east) ;
		THM:description = "either 1) pert moist pot temp=(1+Rv/Rd Qv)*(theta)-T0,
                              or 2) pert dry pot temp=t" ;
		THM:units = "K" ;
	float MU(Time, south_north, west_east) ;
		MU:description = "perturbation dry air mass in column" ;
		MU:units = "Pa" ;
	float MUB(Time, south_north, west_east) ;
		MUB:description = "base state dry air mass in column" ;
		MUB:units = "Pa" ;
	float P(Time, bottom_top, south_north, west_east) ;
		P:description = "perturbation pressure" ;
		P:units = "Pa" ;
	float PB(Time, bottom_top, south_north, west_east) ;
		PB:description = "BASE STATE PRESSURE" ;
		PB:units = "Pa" ;
	float FNM(Time, bottom_top) ;
		FNM:description = "upper weight for vertical stretching" ;
		FNM:units = "" ;
	float FNP(Time, bottom_top) ;
		FNP:description = "lower weight for vertical stretching" ;
		FNP:units = "" ;
	float RDNW(Time, bottom_top) ;
		RDNW:description = "inverse d(eta) values between full (w) levels" ;
		RDNW:units = "" ;
	float RDN(Time, bottom_top) ;
		RDN:description = "inverse d(eta) values between half (mass) levels" ;
		RDN:units = "" ;
	float DNW(Time, bottom_top) ;
		DNW:description = "d(eta) values between full (w) levels" ;
		DNW:units = "" ;
	float DN(Time, bottom_top) ;
		DN:description = "d(eta) values between half (mass) levels" ;
		DN:units = "" ;
	float CFN(Time) ;
		CFN:description = "extrapolation constant" ;
		CFN:units = "" ;
	float CFN1(Time) ;
		CFN1:description = "extrapolation constant" ;
		CFN1:units = "" ;
	int THIS_IS_AN_IDEAL_RUN(Time) ;
		THIS_IS_AN_IDEAL_RUN:description = "T/F flag: this is an ARW ideal simulation" ;
		THIS_IS_AN_IDEAL_RUN:units = "-" ;
	float P_HYD(Time, bottom_top, south_north, west_east) ;
		P_HYD:description = "hydrostatic pressure" ;
		P_HYD:units = "Pa" ;
	float Q2(Time, south_north, west_east) ;
		Q2:description = "QV at 2 M" ;
		Q2:units = "kg kg-1" ;
	float T2(Time, south_north, west_east) ;
		T2:description = "TEMP at 2 M" ;
		T2:units = "K" ;
	float TH2(Time, south_north, west_east) ;
		TH2:description = "POT TEMP at 2 M" ;
		TH2:units = "K" ;
	float PSFC(Time, south_north, west_east) ;
		PSFC:description = "SFC PRESSURE" ;
		PSFC:units = "Pa" ;
	float U10(Time, south_north, west_east) ;
		U10:description = "U at 10 M" ;
		U10:units = "m s-1" ;
	float V10(Time, south_north, west_east) ;
		V10:description = "V at 10 M" ;
		V10:units = "m s-1" ;
	float RDX(Time) ;
		RDX:description = "INVERSE X GRID LENGTH" ;
		RDX:units = "" ;
	float RDY(Time) ;
		RDY:description = "INVERSE Y GRID LENGTH" ;
		RDY:units = "" ;
	float RESM(Time) ;
		RESM:description = "TIME WEIGHT CONSTANT FOR SMALL STEPS" ;
		RESM:units = "" ;
	float CF1(Time) ;
		CF1:description = "2nd order extrapolation constant" ;
		CF1:units = "" ;
	float CF2(Time) ;
		CF2:description = "2nd order extrapolation constant" ;
		CF2:units = "" ;
	float CF3(Time) ;
		CF3:description = "2nd order extrapolation constant" ;
		CF3:units = "" ;
	int ITIMESTEP(Time) ;
		ITIMESTEP:description = "" ;
		ITIMESTEP:units = "" ;
	float XTIME(Time) ;
		XTIME:description = "minutes since 2018-07-14 00:00:00" ;
		XTIME:units = "minutes since 2018-07-14 00:00:00" ;
	float QVAPOR(Time, bottom_top, south_north, west_east) ;
		QVAPOR:description = "Water vapor mixing ratio" ;
		QVAPOR:units = "kg kg-1" ;
	float QCLOUD(Time, bottom_top, south_north, west_east) ;
		QCLOUD:description = "Cloud water mixing ratio" ;
		QCLOUD:units = "kg kg-1" ;
	float QRAIN(Time, bottom_top, south_north, west_east) ;
		QRAIN:description = "Rain water mixing ratio" ;
		QRAIN:units = "kg kg-1" ;
	float QICE(Time, bottom_top, south_north, west_east) ;
		QICE:description = "Ice mixing ratio" ;
		QICE:units = "kg kg-1" ;
	float QSNOW(Time, bottom_top, south_north, west_east) ;
		QSNOW:description = "Snow mixing ratio" ;
		QSNOW:units = "kg kg-1" ;
	float QGRAUP(Time, bottom_top, south_north, west_east) ;
		QGRAUP:description = "Graupel mixing ratio" ;
		QGRAUP:units = "kg kg-1" ;
	float SHDMAX(Time, south_north, west_east) ;
		SHDMAX:description = "ANNUAL MAX VEG FRACTION" ;
		SHDMAX:units = "" ;
	float SHDMIN(Time, south_north, west_east) ;
		SHDMIN:description = "ANNUAL MIN VEG FRACTION" ;
		SHDMIN:units = "" ;
	float SNOALB(Time, south_north, west_east) ;
		SNOALB:description = "ANNUAL MAX SNOW ALBEDO IN FRACTION" ;
		SNOALB:units = "" ;
	float TSLB(Time, soil_layers_stag, south_north, west_east) ;
		TSLB:description = "SOIL TEMPERATURE" ;
		TSLB:units = "K" ;
	float SMOIS(Time, soil_layers_stag, south_north, west_east) ;
		SMOIS:description = "SOIL MOISTURE" ;
		SMOIS:units = "m3 m-3" ;
	float SH2O(Time, soil_layers_stag, south_north, west_east) ;
		SH2O:description = "SOIL LIQUID WATER" ;
		SH2O:units = "m3 m-3" ;
	float SMCREL(Time, soil_layers_stag, south_north, west_east) ;
		SMCREL:description = "RELATIVE SOIL MOISTURE" ;
		SMCREL:units = "" ;
	float SEAICE(Time, south_north, west_east) ;
		SEAICE:description = "SEA ICE FLAG" ;
		SEAICE:units = "" ;
	float XICEM(Time, south_north, west_east) ;
		XICEM:description = "SEA ICE FLAG (PREVIOUS STEP)" ;
		XICEM:units = "" ;
	float SFROFF(Time, south_north, west_east) ;
		SFROFF:description = "SURFACE RUNOFF" ;
		SFROFF:units = "mm" ;
	float UDROFF(Time, south_north, west_east) ;
		UDROFF:description = "UNDERGROUND RUNOFF" ;
		UDROFF:units = "mm" ;
	int IVGTYP(Time, south_north, west_east) ;
		IVGTYP:description = "DOMINANT VEGETATION CATEGORY" ;
		IVGTYP:units = "" ;
	int ISLTYP(Time, south_north, west_east) ;
		ISLTYP:description = "DOMINANT SOIL CATEGORY" ;
		ISLTYP:units = "" ;
	float VEGFRA(Time, south_north, west_east) ;
		VEGFRA:description = "VEGETATION FRACTION" ;
		VEGFRA:units = "" ;
	float GRDFLX(Time, south_north, west_east) ;
		GRDFLX:description = "GROUND HEAT FLUX" ;
		GRDFLX:units = "W m-2" ;
	float ACGRDFLX(Time, south_north, west_east) ;
		ACGRDFLX:description = "ACCUMULATED GROUND HEAT FLUX" ;
		ACGRDFLX:units = "J m-2" ;
	float ACSNOM(Time, south_north, west_east) ;
		ACSNOM:description = "ACCUMULATED MELTED SNOW" ;
		ACSNOM:units = "kg m-2" ;
	float SNOW(Time, south_north, west_east) ;
		SNOW:description = "SNOW WATER EQUIVALENT" ;
		SNOW:units = "kg m-2" ;
	float SNOWH(Time, south_north, west_east) ;
		SNOWH:description = "PHYSICAL SNOW DEPTH" ;
		SNOWH:units = "m" ;
	float CANWAT(Time, south_north, west_east) ;
		CANWAT:description = "CANOPY WATER" ;
		CANWAT:units = "kg m-2" ;
	float SSTSK(Time, south_north, west_east) ;
		SSTSK:description = "SKIN SEA SURFACE TEMPERATURE" ;
		SSTSK:units = "K" ;
	float COSZEN(Time, south_north, west_east) ;
		COSZEN:description = "COS of SOLAR ZENITH ANGLE" ;
		COSZEN:units = "dimensionless" ;
	float LAI(Time, south_north, west_east) ;
		LAI:description = "LEAF AREA INDEX" ;
		LAI:units = "m-2/m-2" ;
	float VAR(Time, south_north, west_east) ;
		VAR:description = "OROGRAPHIC VARIANCE" ;
		VAR:units = "" ;
	float MAPFAC_M(Time, south_north, west_east) ;
		MAPFAC_M:description = "Map scale factor on mass grid" ;
		MAPFAC_M:units = "" ;
	float MAPFAC_U(Time, south_north, west_east_stag) ;
		MAPFAC_U:description = "Map scale factor on u-grid" ;
		MAPFAC_U:units = "" ;
	float MAPFAC_V(Time, south_north_stag, west_east) ;
		MAPFAC_V:description = "Map scale factor on v-grid" ;
		MAPFAC_V:units = "" ;
	float F(Time, south_north, west_east) ;
		F:description = "Coriolis sine latitude term" ;
		F:units = "s-1" ;
	float E(Time, south_north, west_east) ;
		E:description = "Coriolis cosine latitude term" ;
		E:units = "s-1" ;
	float SINALPHA(Time, south_north, west_east) ;
		SINALPHA:description = "Local sine of map rotation" ;
		SINALPHA:units = "" ;
	float COSALPHA(Time, south_north, west_east) ;
		COSALPHA:description = "Local cosine of map rotation" ;
		COSALPHA:units = "" ;
	float HGT(Time, south_north, west_east) ;
		HGT:description = "Terrain Height" ;
		HGT:units = "m" ;
	float TSK(Time, south_north, west_east) ;
		TSK:description = "SURFACE SKIN TEMPERATURE" ;
		TSK:units = "K" ;
	float P_TOP(Time) ;
		P_TOP:description = "PRESSURE TOP OF THE MODEL" ;
		P_TOP:units = "Pa" ;
	float T00(Time) ;
		T00:description = "BASE STATE TEMPERATURE" ;
		T00:units = "K" ;
	float P00(Time) ;
		P00:description = "BASE STATE PRESURE" ;
		P00:units = "Pa" ;
	float TLP(Time) ;
		TLP:description = "BASE STATE LAPSE RATE" ;
		TLP:units = "" ;
	float TISO(Time) ;
		TISO:description = "TEMP AT WHICH THE BASE T TURNS CONST" ;
		TISO:units = "K" ;
	float TLP_STRAT(Time) ;
		TLP_STRAT:description = "BASE STATE LAPSE RATE (DT/D(LN(P)) IN STRATOSPHERE" ;
		TLP_STRAT:units = "K" ;
	float P_STRAT(Time) ;
		P_STRAT:description = "BASE STATE PRESSURE AT BOTTOM OF STRATOSPHERE" ;
		P_STRAT:units = "Pa" ;
	float MAX_MSTFX(Time) ;
		MAX_MSTFX:description = "Max map factor in domain" ;
		MAX_MSTFX:units = "" ;
	float MAX_MSTFY(Time) ;
		MAX_MSTFY:description = "Max map factor in domain" ;
		MAX_MSTFY:units = "" ;
	float RAINC(Time, south_north, west_east) ;
		RAINC:description = "ACCUMULATED TOTAL CUMULUS PRECIPITATION" ;
		RAINC:units = "mm" ;
	float RAINSH(Time, south_north, west_east) ;
		RAINSH:description = "ACCUMULATED SHALLOW CUMULUS PRECIPITATION" ;
		RAINSH:units = "mm" ;
	float RAINNC(Time, south_north, west_east) ;
		RAINNC:description = "ACCUMULATED TOTAL GRID SCALE PRECIPITATION" ;
		RAINNC:units = "mm" ;
	float SNOWNC(Time, south_north, west_east) ;
		SNOWNC:description = "ACCUMULATED TOTAL GRID SCALE SNOW AND ICE" ;
		SNOWNC:units = "mm" ;
	float GRAUPELNC(Time, south_north, west_east) ;
		GRAUPELNC:description = "ACCUMULATED TOTAL GRID SCALE GRAUPEL" ;
		GRAUPELNC:units = "mm" ;
	float HAILNC(Time, south_north, west_east) ;
		HAILNC:description = "ACCUMULATED TOTAL GRID SCALE HAIL" ;
		HAILNC:units = "mm" ;
	float REFL_10CM(Time, bottom_top, south_north, west_east) ;
		REFL_10CM:description = "Radar reflectivity (lamda = 10 cm)" ;
		REFL_10CM:units = "dBZ" ;
	float CLDFRA(Time, bottom_top, south_north, west_east) ;
		CLDFRA:description = "CLOUD FRACTION" ;
		CLDFRA:units = "" ;
	float SWDOWN(Time, south_north, west_east) ;
		SWDOWN:description = "DOWNWARD SHORT WAVE FLUX AT GROUND SURFACE" ;
		SWDOWN:units = "W m-2" ;
	float GLW(Time, south_north, west_east) ;
		GLW:description = "DOWNWARD LONG WAVE FLUX AT GROUND SURFACE" ;
		GLW:units = "W m-2" ;
	float SWNORM(Time, south_north, west_east) ;
		SWNORM:description = "NORMAL SHORT WAVE FLUX AT GROUND SURFACE (SLOPE-DEPENDENT)" ;
		SWNORM:units = "W m-2" ;
	float ACSWUPT(Time, south_north, west_east) ;
		ACSWUPT:description = "ACCUMULATED UPWELLING SHORTWAVE FLUX AT TOP" ;
		ACSWUPT:units = "J m-2" ;
	float ACSWUPTC(Time, south_north, west_east) ;
		ACSWUPTC:description = "ACCUMULATED UPWELLING CLEAR SKY SHORTWAVE FLUX AT TOP" ;
		ACSWUPTC:units = "J m-2" ;
	float ACSWDNT(Time, south_north, west_east) ;
		ACSWDNT:description = "ACCUMULATED DOWNWELLING SHORTWAVE FLUX AT TOP" ;
		ACSWDNT:units = "J m-2" ;
	float ACSWDNTC(Time, south_north, west_east) ;
		ACSWDNTC:description = "ACCUMULATED DOWNWELLING CLEAR SKY SHORTWAVE FLUX AT TOP" ;
		ACSWDNTC:units = "J m-2" ;
	float ACSWUPB(Time, south_north, west_east) ;
		ACSWUPB:description = "ACCUMULATED UPWELLING SHORTWAVE FLUX AT BOTTOM" ;
		ACSWUPB:units = "J m-2" ;
	float ACSWUPBC(Time, south_north, west_east) ;
		ACSWUPBC:description = "ACCUMULATED UPWELLING CLEAR SKY SHORTWAVE FLUX AT BOTTOM" ;
		ACSWUPBC:units = "J m-2" ;
	float ACSWDNB(Time, south_north, west_east) ;
		ACSWDNB:description = "ACCUMULATED DOWNWELLING SHORTWAVE FLUX AT BOTTOM" ;
		ACSWDNB:units = "J m-2" ;
	float ACSWDNBC(Time, south_north, west_east) ;
		ACSWDNBC:description = "ACCUMULATED DOWNWELLING CLEAR SKY SHORTWAVE FLUX AT BOTTOM" ;
		ACSWDNBC:units = "J m-2" ;
	float ACLWUPT(Time, south_north, west_east) ;
		ACLWUPT:description = "ACCUMULATED UPWELLING LONGWAVE FLUX AT TOP" ;
		ACLWUPT:units = "J m-2" ;
	float ACLWUPTC(Time, south_north, west_east) ;
		ACLWUPTC:description = "ACCUMULATED UPWELLING CLEAR SKY LONGWAVE FLUX AT TOP" ;
		ACLWUPTC:units = "J m-2" ;
	float ACLWDNT(Time, south_north, west_east) ;
		ACLWDNT:description = "ACCUMULATED DOWNWELLING LONGWAVE FLUX AT TOP" ;
		ACLWDNT:units = "J m-2" ;
	float ACLWDNTC(Time, south_north, west_east) ;
		ACLWDNTC:description = "ACCUMULATED DOWNWELLING CLEAR SKY LONGWAVE FLUX AT TOP" ;
		ACLWDNTC:units = "J m-2" ;
	float ACLWUPB(Time, south_north, west_east) ;
		ACLWUPB:description = "ACCUMULATED UPWELLING LONGWAVE FLUX AT BOTTOM" ;
		ACLWUPB:units = "J m-2" ;
	float ACLWUPBC(Time, south_north, west_east) ;
		ACLWUPBC:description = "ACCUMULATED UPWELLING CLEAR SKY LONGWAVE FLUX AT BOTTOM" ;
		ACLWUPBC:units = "J m-2" ;
	float ACLWDNB(Time, south_north, west_east) ;
		ACLWDNB:description = "ACCUMULATED DOWNWELLING LONGWAVE FLUX AT BOTTOM" ;
		ACLWDNB:units = "J m-2" ;
	float ACLWDNBC(Time, south_north, west_east) ;
		ACLWDNBC:description = "ACCUMULATED DOWNWELLING CLEAR SKY LONGWAVE FLUX AT BOTTOM" ;
		ACLWDNBC:units = "J m-2" ;
	float SWUPT(Time, south_north, west_east) ;
		SWUPT:description = "INSTANTANEOUS UPWELLING SHORTWAVE FLUX AT TOP" ;
		SWUPT:units = "W m-2" ;
	float SWUPTC(Time, south_north, west_east) ;
		SWUPTC:description = "INSTANTANEOUS UPWELLING CLEAR SKY SHORTWAVE FLUX AT TOP" ;
		SWUPTC:units = "W m-2" ;
	float SWDNT(Time, south_north, west_east) ;
		SWDNT:description = "INSTANTANEOUS DOWNWELLING SHORTWAVE FLUX AT TOP" ;
		SWDNT:units = "W m-2" ;
	float SWDNTC(Time, south_north, west_east) ;
		SWDNTC:description = "INSTANTANEOUS DOWNWELLING CLEAR SKY SHORTWAVE FLUX AT TOP" ;
		SWDNTC:units = "W m-2" ;
	float SWUPB(Time, south_north, west_east) ;
		SWUPB:description = "INSTANTANEOUS UPWELLING SHORTWAVE FLUX AT BOTTOM" ;
		SWUPB:units = "W m-2" ;
	float SWUPBC(Time, south_north, west_east) ;
		SWUPBC:description = "INSTANTANEOUS UPWELLING CLEAR SKY SHORTWAVE FLUX AT BOTTOM" ;
		SWUPBC:units = "W m-2" ;
	float SWDNB(Time, south_north, west_east) ;
		SWDNB:description = "INSTANTANEOUS DOWNWELLING SHORTWAVE FLUX AT BOTTOM" ;
		SWDNB:units = "W m-2" ;
	float SWDNBC(Time, south_north, west_east) ;
		SWDNBC:description = "INSTANTANEOUS DOWNWELLING CLEAR SKY SHORTWAVE FLUX AT BOTTOM" ;
		SWDNBC:units = "W m-2" ;
	float LWUPT(Time, south_north, west_east) ;
		LWUPT:description = "INSTANTANEOUS UPWELLING LONGWAVE FLUX AT TOP" ;
		LWUPT:units = "W m-2" ;
	float LWUPTC(Time, south_north, west_east) ;
		LWUPTC:description = "INSTANTANEOUS UPWELLING CLEAR SKY LONGWAVE FLUX AT TOP" ;
		LWUPTC:units = "W m-2" ;
	float LWDNT(Time, south_north, west_east) ;
		LWDNT:description = "INSTANTANEOUS DOWNWELLING LONGWAVE FLUX AT TOP" ;
		LWDNT:units = "W m-2" ;
	float LWDNTC(Time, south_north, west_east) ;
		LWDNTC:description = "INSTANTANEOUS DOWNWELLING CLEAR SKY LONGWAVE FLUX AT TOP" ;
		LWDNTC:units = "W m-2" ;
	float LWUPB(Time, south_north, west_east) ;
		LWUPB:description = "INSTANTANEOUS UPWELLING LONGWAVE FLUX AT BOTTOM" ;
		LWUPB:units = "W m-2" ;
	float LWUPBC(Time, south_north, west_east) ;
		LWUPBC:description = "INSTANTANEOUS UPWELLING CLEAR SKY LONGWAVE FLUX AT BOTTOM" ;
		LWUPBC:units = "W m-2" ;
	float LWDNB(Time, south_north, west_east) ;
		LWDNB:description = "INSTANTANEOUS DOWNWELLING LONGWAVE FLUX AT BOTTOM" ;
		LWDNB:units = "W m-2" ;
	float LWDNBC(Time, south_north, west_east) ;
		LWDNBC:description = "INSTANTANEOUS DOWNWELLING CLEAR SKY LONGWAVE FLUX AT BOTTOM" ;
		LWDNBC:units = "W m-2" ;
	float OLR(Time, south_north, west_east) ;
		OLR:description = "TOA OUTGOING LONG WAVE" ;
		OLR:units = "W m-2" ;
	float XLAT_U(Time, south_north, west_east_stag) ;
		XLAT_U:description = "LATITUDE, SOUTH IS NEGATIVE" ;
		XLAT_U:units = "degree_north" ;
	float XLONG_U(Time, south_north, west_east_stag) ;
		XLONG_U:description = "LONGITUDE, WEST IS NEGATIVE" ;
		XLONG_U:units = "degree_east" ;
	float XLAT_V(Time, south_north_stag, west_east) ;
		XLAT_V:description = "LATITUDE, SOUTH IS NEGATIVE" ;
		XLAT_V:units = "degree_north" ;
	float XLONG_V(Time, south_north_stag, west_east) ;
		XLONG_V:description = "LONGITUDE, WEST IS NEGATIVE" ;
		XLONG_V:units = "degree_east" ;
	float ALBEDO(Time, south_north, west_east) ;
		ALBEDO:description = "ALBEDO" ;
		ALBEDO:units = "-" ;
	float CLAT(Time, south_north, west_east) ;
		CLAT:description = "COMPUTATIONAL GRID LATITUDE, SOUTH IS NEGATIVE" ;
		CLAT:units = "degree_north" ;
	float ALBBCK(Time, south_north, west_east) ;
		ALBBCK:description = "BACKGROUND ALBEDO" ;
		ALBBCK:units = "" ;
	float EMISS(Time, south_north, west_east) ;
		EMISS:description = "SURFACE EMISSIVITY" ;
		EMISS:units = "" ;
	float NOAHRES(Time, south_north, west_east) ;
		NOAHRES:description = "RESIDUAL OF THE NOAH SURFACE ENERGY BUDGET" ;
		NOAHRES:units = "W m{-2}" ;
	float TMN(Time, south_north, west_east) ;
		TMN:description = "SOIL TEMPERATURE AT LOWER BOUNDARY" ;
		TMN:units = "K" ;
	float XLAND(Time, south_north, west_east) ;
		XLAND:description = "LAND MASK (1 FOR LAND, 2 FOR WATER)" ;
		XLAND:units = "" ;
	float UST(Time, south_north, west_east) ;
		UST:description = "U* IN SIMILARITY THEORY" ;
		UST:units = "m s-1" ;
	float PBLH(Time, south_north, west_east) ;
		PBLH:description = "PBL HEIGHT" ;
		PBLH:units = "m" ;
	float HFX(Time, south_north, west_east) ;
		HFX:description = "UPWARD HEAT FLUX AT THE SURFACE" ;
		HFX:units = "W m-2" ;
	float QFX(Time, south_north, west_east) ;
		QFX:description = "UPWARD MOISTURE FLUX AT THE SURFACE" ;
		QFX:units = "kg m-2 s-1" ;
	float LH(Time, south_north, west_east) ;
		LH:description = "LATENT HEAT FLUX AT THE SURFACE" ;
		LH:units = "W m-2" ;
	float ACHFX(Time, south_north, west_east) ;
		ACHFX:description = "ACCUMULATED UPWARD HEAT FLUX AT THE SURFACE" ;
		ACHFX:units = "J m-2" ;
	float ACLHF(Time, south_north, west_east) ;
		ACLHF:description = "ACCUMULATED UPWARD LATENT HEAT FLUX AT THE SURFACE" ;
		ACLHF:units = "J m-2" ;
	float SNOWC(Time, south_north, west_east) ;
		SNOWC:description = "FLAG INDICATING SNOW COVERAGE (1 FOR SNOW COVER)" ;
		SNOWC:units = "" ;
	float SR(Time, south_north, west_east) ;
		SR:description = "fraction of frozen precipitation" ;
		SR:units = "-" ;
	float C1H(Time, bottom_top) ;
		C1H:description = "half levels, c1h = d bf / d eta, using znw" ;
		C1H:units = "Dimensionless" ;
	float C2H(Time, bottom_top) ;
		C2H:description = "half levels, c2h = (1-c1h)*(p0-pt)" ;
		C2H:units = "Pa" ;
	float C1F(Time, bottom_top_stag) ;
		C1F:description = "full levels, c1f = d bf / d eta, using znu" ;
		C1F:units = "Dimensionless" ;
	float C2F(Time, bottom_top_stag) ;
		C2F:description = "full levels, c2f = (1-c1f)*(p0-pt)" ;
		C2F:units = "Pa" ;
	float C3H(Time, bottom_top) ;
		C3H:description = "half levels, c3h = bh" ;
		C3H:units = "Dimensionless" ;
	float C4H(Time, bottom_top) ;
		C4H:description = "half levels, c4h = (eta-bh)*(p0-pt)+pt, using znu" ;
		C4H:units = "Pa" ;
	float C3F(Time, bottom_top_stag) ;
		C3F:description = "full levels, c3f = bf" ;
		C3F:units = "Dimensionless" ;
	float C4F(Time, bottom_top_stag) ;
		C4F:description = "full levels, c4f = (eta-bf)*(p0-pt)+pt, using znw" ;
		C4F:units = "Pa" ;
	float PCB(Time, south_north, west_east) ;
		PCB:description = "base state dry air mass in column" ;
		PCB:units = "Pa" ;
	float PC(Time, south_north, west_east) ;
		PC:description = "perturbation dry air mass in column" ;
		PC:units = "Pa" ;
	float LANDMASK(Time, south_north, west_east) ;
		LANDMASK:description = "LAND MASK (1 FOR LAND, 0 FOR WATER)" ;
		LANDMASK:units = "" ;
	float LAKEMASK(Time, south_north, west_east) ;
		LAKEMASK:description = "LAKE MASK (1 FOR LAKE, 0 FOR NON-LAKE)" ;
		LAKEMASK:units = "" ;
	float SST(Time, south_north, west_east) ;
		SST:description = "SEA SURFACE TEMPERATURE" ;
		SST:units = "K" ;

全局属性列表 List of Global Attributes 
		:TITLE = " OUTPUT FROM WRF V4.0.3 MODEL" ;
		:START_DATE = "2018-07-14_00:00:00" ;
		:SIMULATION_START_DATE = "2018-07-14_00:00:00" ;
		:WEST-EAST_GRID_DIMENSION = 501 ;
		:SOUTH-NORTH_GRID_DIMENSION = 501 ;
		:BOTTOM-TOP_GRID_DIMENSION = 56 ;
		:DX = 4000.f ;
		:DY = 4000.f ;
		:AERCU_OPT = 0 ;
		:AERCU_FCT = 1.f ;
		:IDEAL_CASE = 0 ;
		:DIFF_6TH_SLOPEOPT = 0 ;
		:AUTO_LEVELS_OPT = 2 ;
		:DIFF_6TH_THRESH = 0.1f ;
		:DZBOT = 50.f ;
		:DZSTRETCH_S = 1.3f ;
		:DZSTRETCH_U = 1.1f ;
		:SKEBS_ON = 0 ;
		:SPEC_BDY_FINAL_MU = 1 ;
		:USE_Q_DIABATIC = 0 ;
		:GRIDTYPE = "C" ;
		:DIFF_OPT = 1 ;
		:KM_OPT = 4 ;
		:DAMP_OPT = 3 ;
		:DAMPCOEF = 0.2f ;
		:KHDIF = 0.f ;
		:KVDIF = 0.f ;
		:MP_PHYSICS = 6 ;
		:RA_LW_PHYSICS = 4 ;
		:RA_SW_PHYSICS = 4 ;
		:SF_SFCLAY_PHYSICS = 1 ;
		:SF_SURFACE_PHYSICS = 2 ;
		:BL_PBL_PHYSICS = 1 ;
		:CU_PHYSICS = 0 ;
		:SF_LAKE_PHYSICS = 0 ;
		:SURFACE_INPUT_SOURCE = 3 ;
		:SST_UPDATE = 0 ;
		:GRID_FDDA = 0 ;
		:GFDDA_INTERVAL_M = 0 ;
		:GFDDA_END_H = 0 ;
		:GRID_SFDDA = 0 ;
		:SGFDDA_INTERVAL_M = 0 ;
		:SGFDDA_END_H = 0 ;
		:HYPSOMETRIC_OPT = 2 ;
		:USE_THETA_M = 1 ;
		:GWD_OPT = 0 ;
		:SF_URBAN_PHYSICS = 0 ;
		:SF_SURFACE_MOSAIC = 0 ;
		:SF_OCEAN_PHYSICS = 0 ;
		:SHCU_PHYSICS = 0 ;
		:MFSHCONV = 0 ;
		:FEEDBACK = 1 ;
		:SMOOTH_OPTION = 2 ;
		:SWRAD_SCAT = 1.f ;
		:W_DAMPING = 0 ;
		:RADT = 15.f ;
		:BLDT = 0.f ;
		:CUDT = 0.f ;
		:AER_OPT = 0 ;
		:SWINT_OPT = 0 ;
		:AER_TYPE = 1 ;
		:AER_AOD550_OPT = 1 ;
		:AER_ANGEXP_OPT = 1 ;
		:AER_SSA_OPT = 1 ;
		:AER_ASY_OPT = 1 ;
		:AER_AOD550_VAL = 0.12f ;
		:AER_ANGEXP_VAL = 1.3f ;
		:AER_SSA_VAL = 0.85f ;
		:AER_ASY_VAL = 0.9f ;
		:MOIST_ADV_OPT = 1 ;
		:SCALAR_ADV_OPT = 1 ;
		:TKE_ADV_OPT = 1 ;
		:DIFF_6TH_OPT = 0 ;
		:DIFF_6TH_FACTOR = 0.12f ;
		:OBS_NUDGE_OPT = 0 ;
		:BUCKET_MM = -1.f ;
		:BUCKET_J = -1.f ;
		:PREC_ACC_DT = 0.f ;
		:ISFTCFLX = 0 ;
		:ISHALLOW = 0 ;
		:ISFFLX = 1 ;
		:ICLOUD = 1 ;
		:ICLOUD_CU = 0 ;
		:TRACER_PBLMIX = 1 ;
		:SCALAR_PBLMIX = 0 ;
		:YSU_TOPDOWN_PBLMIX = 0 ;
		:GRAV_SETTLING = 0 ;
		:DFI_OPT = 0 ;
		:SIMULATION_INITIALIZATION_TYPE = "REAL-DATA CASE" ;
		:WEST-EAST_PATCH_START_UNSTAG = 1 ;
		:WEST-EAST_PATCH_END_UNSTAG = 500 ;
		:WEST-EAST_PATCH_START_STAG = 1 ;
		:WEST-EAST_PATCH_END_STAG = 501 ;
		:SOUTH-NORTH_PATCH_START_UNSTAG = 1 ;
		:SOUTH-NORTH_PATCH_END_UNSTAG = 500 ;
		:SOUTH-NORTH_PATCH_START_STAG = 1 ;
		:SOUTH-NORTH_PATCH_END_STAG = 501 ;
		:BOTTOM-TOP_PATCH_START_UNSTAG = 1 ;
		:BOTTOM-TOP_PATCH_END_UNSTAG = 55 ;
		:BOTTOM-TOP_PATCH_START_STAG = 1 ;
		:BOTTOM-TOP_PATCH_END_STAG = 56 ;
		:GRID_ID = 1 ;
		:PARENT_ID = 0 ;
		:I_PARENT_START = 1 ;
		:J_PARENT_START = 1 ;
		:PARENT_GRID_RATIO = 1 ;
		:DT = 20.f ;
		:CEN_LAT = 39.00001f ;
		:CEN_LON = -98.f ;
		:TRUELAT1 = 30.f ;
		:TRUELAT2 = 50.f ;
		:MOAD_CEN_LAT = 39.00001f ;
		:STAND_LON = -98.f ;
		:POLE_LAT = 90.f ;
		:POLE_LON = 0.f ;
		:GMT = 0.f ;
		:JULYR = 2018 ;
		:JULDAY = 195 ;
		:MAP_PROJ = 1 ;
		:MAP_PROJ_CHAR = "Lambert Conformal" ;
		:MMINLU = "MODIFIED_IGBP_MODIS_NOAH" ;
		:NUM_LAND_CAT = 21 ;
		:ISWATER = 17 ;
		:ISLAKE = 21 ;
		:ISICE = 15 ;
		:ISURBAN = 13 ;
		:ISOILWATER = 14 ;
		:HYBRID_OPT = 2 ;
		:ETAC = 0.2f ; 

<a id=Special_Output></a>

### 特殊的WRF输出变量

WRF模型输出在注册表文件中定义的状态变量，这些状态变量用于模型的预测方程式中。这些变量中的一些是摄动场。因此，以下用于重建气象变量的定义是必要的：
total geopotential总地势	PH + PHB
total geopotential height in m总地势高度，以米为单位	( PH + PHB ) / 9.81
total potential temperature in_ K总潜在温度	T + 300
total pressure in mb总压力	( P + PB ) * 0.01
wind compoments, grid relative风洞，相对网格	U, V
surface pressure in Pa表面压力	psfc
surface winds, grid relative地面风，相对网格	U10, V10 (valid at mass points)
surface temperature and mixing ratio表面温度和混合比	T2, Q2

地图投影选项的定义：
map_proj = 1：兰伯特共形  Lambert Conformal           
            2：极地立体照相 Polar Stereographic
            3：墨卡托 Mercator
            6：经纬度（包括全球范围）latitude and longitude (including global)

