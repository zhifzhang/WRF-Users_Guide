# 第二章 软件安装

## 目录

1. [简介](#Introduction)

2. [需要的编译器和脚本语言](#Compilers_Scripting)

3. [需要/可选的库文件](#Libraries)

4. [后处理工具](#Post_Processing)

5. [UNIX环境设置](#UNIX_Environment)

6. [编译WRF核心](#Building_WRF)

7. [编译WPS核心](#Building_WPS)

<a id=Introduction></a>

## 简介

[WRF](http://www.mmm.ucar.edu/wrf/users )模型系统[软件](http://www.mmm.ucar.edu/wrf/users/download/get_source.html )安装在下面列出的平台上相当简单。包的模型组件部分大部分是独立的。WRF模型包含到ESMF的Fortran接口的源代码和到FFTPACK的源代码。WRF系统中包含WRFDA组件，它有几个用户必须安装的外部库（用于各种观测类型和线性代数解算器）。类似地，WPS包与WRF源代码分离，必须编译额外的外部库（以支持Grib2处理）。所有系统都需要的一个外部包是netCDF库，它是受支持的I/O API包之一。netCDF库和源代码可从[Unidata主页]( http://www.unidata.ucar.edu )获取。

WRF代码有三个tar文件。第一个是WRF模型（包括real和ideal预处理器），第二个是WRFDA代码，第三个是WRF化学。为了运行WRF化学代码，必须将WRF模型和化学tar文件结合起来。

WRF模型已经成功地移植到许多基于Unix的系统上。我们不能访问所有这些选项，必须依赖外部用户和供应商为编译器和加载程序选项提供所需的配置信息。下面是WRF支持的硬件和软件组合列表。

**销售商**|**硬件**|**操作系统**|**编译器**
----------|--------|-------------|-----------
Cray | XC30 Intel | Linux | Intel
Cray | XE AMD | Linux | Intel
IBM | Power Series | AIX | vendor
IBM | Intel | Linux | Intel/PGI/gfortran
SGI | IA64/Opteron | Linux | Intel
COTS（Commercial Off-The-Shelf systems） | IA32 | Linux | Intel/PGI/gfortran/g95/PathScale
COTS | IA64/Opteron | Linux | Intel/PGI/gfortran/PathScale
Mac | Power Series | Darwin | xlf/g95/PGI/Intel
Mac | Intel | Darwin | gfortran/PGI/Intel
NEC | NEC | Linux | vendor
Fujitsu | FX10 Intel | Linux | vendor

WRF模型可以在单处理器机器、共享内存机器（使用OpenMP API）、分布式内存机器（具有适当的MPI库）或分布式集群（同时使用OpenMP和MPI）上编译运行。WRFDA和WPS包在上面列出的系统上运行。

<a id=Compilers_Scripting></a>

## 需要的编译器和脚本语言

大多数WRF模型、WPS和WRFDA代码都是用Fortran编写的（称之为fortran90）。位于WRF和WRFDA之间的软件层[RSL](http://www-unix.mcs.anl.gov/~michalak/rsl )和MPI接口用C语言编写。WPS直接调用MPI库来传递分布式内存消息。还有一些用C编写的辅助程序来执行文件解析和文件构造，这是默认构建WRF建模代码所必需的。此外，WRF编译机制使用了几种脚本语言：包括[perl](http://www.perl.com/download.csp )、Cshell和Bourne shell。同时还使用传统的UNIX文本/文件处理实用程序：make、m4、sed和awk。请参阅第8章：[WRF软件]( users_guide_chap8.md )（必需的软件）以获取WRF编译所需的更详细的列表。

<a id=Libraries></a>

## 需要/可选的库文件

唯一必须的库是[Unidata]( http://www.unidata.ucar.edu )的netCDF包。大多数WRF后处理包都假设来自WRF模型、WPS包或WRFDA程序的数据正在使用netCDF库。可能还需要将“/path-to-netcdf/netcdf/bin”添加到环境路径中，以便执行netCDF实用程序命令，例如**ncdump**。至少应该使用3.6.1或更高版本的netCDF。**如果要使用压缩功能，请使用netCDF 4.0或更高版本。请注意，压缩功能还需要使用HDF5。**

**注意（1）**：如果要在可以访问多个编译器的Linux或Darwin系统上编译WRF系统组件，请链接正确的外部库。例如，在使用gfortran编译WRF组件时，不要链接使用PathScale构建的库。更重要的是，必须使用构建netCDF库时相同的选项来构建WRF代码（32位或64位、关于符号名中下划线的假设、等等）。

**注意（2）**：如果使用netCDF-4，请确保安装时未激活基于HDF5的并行I/O。WRF建模系统可以使用netCDF-3中的经典数据模型，也可以使用netCDF-4中支持的压缩选项。

如果要运行分布式内存WRF作业，则需要一个MPI版本。您可以选择一个版本的[mpich](http://www.mcs.anl.gov/research/projects/mpich2 )，但您可能希望您的系统组安装该代码。在使用分布式内存构建WRF之前，需要安装MPI。MPI-1或MPI-2均可接受。你已经有MPI了吗？可以尝试以下命令：

```
which mpif90
which mpicc
which mpirun
```

如果所有这些可执行文件都在您的路径中定义了，那么就可以了。确保路径设置为指向MPI lib、include和bin目录。和netCDF一样，您必须使用WRF源代码一致地构建MPI。
 
请注意，为了以Grib1格式输出WRF模型数据，Todd Hutchinson（[WSI](http://www.wsi.com/ )）提供了一个完整的源代码库，该库包含在软件版本中。但是，在尝试将WPS、WRF模型和WRFDA数据流链接在一起时，应始终使用netCDF格式。

**注意（3）**：构建WRF和WPS包的整个步骤可从[此网址](http://www2.mmm.ucar.edu/wrf/OnLineTutorial/compilation_tutorial.php )获得，这个页面包括完整的交钥匙方向，从测试您的机器的实用程序一直到哪里下载实时数据。

<a id=Post_Processing></a>

## 后处理工具

更广泛使用（支持）的WRF后处理实用程序包括：

	NCL([主页](http://www.ncl.ucar.edu/ )和[WRF下载](http://www.mmm.ucar.edu/wrf/users/download/get_source.html )）
		NCAR命令语言由NCAR的计算机信息系统实验室(前身为科学计算部门)编写
		由WRF支持编写和维护NCL脚本
		提供了许多[模板脚本](http://www2.mmm.ucar.edu/wrf/OnLineTutorial/Graphics/NCL/NCL_examples.htm )，这些脚本是针对特定的real-数据和ideal-数据案例进行定制的
		raw WRF output can be input with the NCL scripts
		interactive or command-file driven

	GrADS (homepage and WRF download)
		download GrADS executable, build format converter
		programs (ARWpost) are available to convert the WRF output into an input format suitable for GrADS
		simple to generate publication quality
		interactive or command-file driven

	RIP4 (homepage and WRF download)
		RIP4 written and maintained by Mark Stoelinga, UW
		interpolation to various surfaces, trajectories, hundreds of diagnostic calculations
		Fortran source provided
		based on the NCAR Graphics package
		pre-processor converts WRF, WPS, and WRFDA data to RIP input format
		table driven

<a id=UNIX_Environment></a>

## UNIX环境设置

There are only a few environmental settings that are WRF system related. Most of these are not required, but when things start acting badly, test some out. In Cshell syntax:

·      setenv WRF_EM_CORE 1

o   explicitly defines which model core to build

·      setenv WRF_NMM_CORE 0

explicitly defines which model core NOT to build
·      setenv WRF_DA_CORE 0

o   explicitly defines no data assimilation

·      setenv NETCDF /usr/local/netcdf (or wherever you have it stored)

o   all of the WRF components want both the lib and the include directories

·      setenv OMP_NUM_THREADS n (where n is the number of procs to use)

if you have OpenMP on your system, this is how to specify the number of threads
·      setenv MP_STACK_SIZE 64000000

o   OpenMP blows through the stack size, set it large

o   However, if the model still crashes, it may be a problem of over- specifying stack size. Set stack size sufficiently large, but not unlimited.

o   On some systems, the equivalent parameter could be KMP_STACKSIZE, or OMP_STACKSIZE

·      unlimit

o   especially if you are on a small system

<a id=Building_WRF></a>

## 编译WRF核心

The WRF code has a fairly complicated build mechanism. It tries to determine the architecture that you are on, and then presents you with options to allow you to select the preferred build method. For example, if you are on a Linux machine, it determines whether this is a 32 or 64 bit machine, and then prompts you for the desired usage of processors (such as serial, shared memory, or distributed memory).  You select from among the available compiling options in the build mechanism.  For example, do not choose a PGI build if you do not have PGI compilers installed on your system.

An instructional web site describes the sequence of steps required to build the WRF and WPS codes (though the instructions are specifically given for tcsh and GNU compilers).

http://www2.mmm.ucar.edu/wrf/OnLineTutorial/compilation_tutorial.php

Get the WRF zipped tar file for WRFV4 from
http://www2.mmm.ucar.edu/wrf/users/download/get_source.html
Always get the latest version if you are not trying to continue a long project, or duplicate previous work
unzip and untar the file
gzip -cd WRFV4.0.TAR.gz | tar -xf –
Alternatively tar –xzf WRFV4.0.TAR.gz on some systems
cd WRF
./configure
serial means single processor
smpar means Symmetric Multi-Processing/Shared Memory Parallel (OpenMP) – this does not reliably work on most non-IBM machines
dmpar means Distributed Memory Parallel (MPI)
dm+sm means Distributed Memory with Shared Memory (for example, MPI across nodes with OpenMP within a node) – usually better performance is through dmpar only
The second option is for nesting: 0 = no nesting, 1 = standard static nesting, 2 = nesting with a prescribed set of moves, 3 = nesting that allows a domain to follow a vortex (typhoon tracking)
A typical option that may be included on the ./configure command is the flag “-d” (for debug).  This option removes optimization, which is useful when running a debugger (such as gdb or dbx)
For bounds checking and some additional exception handling, the debugging flag “-D” may be selected.  Only PGI, Intel, and gfortran have been set up to use this option.
./compile em_real (or any of the directory names in ./WRF/test directory)
ls -ls main/*.exe
If you built a real-data case, you should see ndown.exe, real.exe, and wrf.exe
If you built an ideal-data case, you should see ideal.exe and wrf.exe

The WRF code supports a parallel build option, an option that compiles separate source code files in the WRF directories at the same time on separate processors (though those processors need to share memory) via a parallel make.  The purpose of the parallel build option is to be able to speed-up the time required to construct executables.  In practice, users typically see approximately a 2x speed-up, a limit imposed by the various dependencies in the code due to modules and USE association.  To enable the parallel build option, the user sets an environment variable, J.  In csh, to utilize two processors, before the ./compile command, issue the following:

setenv J “-j 2”

Users may wish to only use a single processor for the build.  In which case:

setenv J “-j 1”

Users wishing to run the WRF chemistry code must first download the WRF model tar file, and untar it.  Then the chemistry code is untar’ed in the WRF directory (this is the chem directory structure).  Once the source code from the tar files is combined, then users may proceed with the WRF chemistry build.

<a id=Building_WPS></a>

## 编译WPS核心

Building WPS requires that WRF be already built.

If you plan to use Grib2 data, additional libraries for zlib, png, and jasper are required.  Please see details in Chapter 3.

Get the WPS zipped tar file WPSV4.0.TAR.gz from 
http://www2.mmm.ucar.edu/wrf/users/download/get_source.html
Also download the geographical datasets from the same page. There are new data sets for land cover for North America (NLCD), and high-resolution urban data sets for select North American cities.
Unzip and untar the source code file
gzip -cd WPSV4.0.TAR.gz | tar -xf -
cd WPS
./configure
Choose one of the options
Usually, serial builds are the best for an initial test. Most large domains work with a single processor for WPS
WPS requires that you build for the appropriate Grib decoding. Select an option that is suitable for the data you will use with the ungrib program (the Grib2 option will work for either Grib1 or Grib2 data)
If you select a Grib2 option, you must have those libraries prepared and built in advance (see the chapter on WPS for the location of these compression libraries).  Add the paths to these libraries and include files using variables COMPRESSION_LIBS and COMPRESSION_INC in configure.wps. Also inside the configure.wps file is the location of the built WRF directory, which needs to be modified.  This is how the WPS picks up all of the required IO pieces to build the geogrid.exe and metgrid.exe files.
./compile
ls -ls *.exe
You should see geogrid.exe, ungrib.exe, and metgrid.exe (if you are missing both geogrid.exe and metgrid.exe, you probably need to fix where the path to WRF is pointing in the configure.wps file; if you are missing ungrib.exe, try a Grib1-only build to further isolate the problem)
 

ls -ls util/*.exe
You should see a number of utility executables: avg_tsfc.exe, calc_ecmwf_p.exe, g1print.exe, g2print.exe, height_ukmo.exe, mod_levs.exe, plotfmt.exe, plotgrids.exe, and rd_intermediate.exe (files requiring NCAR Graphics are plotfmt.exe and plotgrids.exe)
If geogrid.exe and metgrid.exe executables are missing, the path to the built WRF directory structure is probably incorrect (found inside the configure.wps file)
If the ungrib.exe is missing, the Grib2 libraries are probably not linked or built correctly
If  the plotfmt.exe or the plotgrids.exe programs is missing, the NCAR Graphics path is probably set incorrectly