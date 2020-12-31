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

* NCL([主页](http://www.ncl.ucar.edu/ )和[WRF下载](http://www.mmm.ucar.edu/wrf/users/download/get_source.html )）
	* NCAR命令语言由NCAR的计算机信息系统实验室(前身为科学计算部门)编写
	* 由WRF支持编写和维护NCL脚本
	* 提供了许多[模板脚本](http://www2.mmm.ucar.edu/wrf/OnLineTutorial/Graphics/NCL/NCL_examples.htm )，这些脚本是针对特定的real-数据和ideal-数据案例进行定制的
	* 原始的WRF输出文件可用于NCL脚本输入
	* 交互式或命令文件驱动

* GrADS([主页](http://grads.iges.org/grads/grads.html )和[WRF下载](http://www.mmm.ucar.edu/wrf/users/download/get_source.html )）
	* 下载GrADS可执行文件，构建格式转换器
	* 程序（ARWpost）可用于将WRF输出转换为适合GrADS的输入格式
	* 易于生成出版物质量的图形
	* 交互式或命令文件驱动

* RIP4([主页](http://www.mmm.ucar.edu/wrf/users/docs/ripug.htm )和[WRF下载](http://www.mmm.ucar.edu/wrf/users/download/get_source.html )）
	* RIP4由UW的Mark Stoelinga编写和维护
	* 插值到各种表面、轨迹、数百种诊断计算
	* 提供了Fortran源代码
	* 基于NCAR图形包
	* 预处理器将WRF、WPS和WRFDA数据转换为RIP输入格式
	* 表驱动

<a id=UNIX_Environment></a>

## UNIX环境设置

与WRF系统相关的环境设置很少，其中大多数不是必需的，如果发生问题，请检查Cshell中的以下设置：

`setenv WRF_EM_CORE 1`

	* 明确定义要构建的模型核心

`setenv WRF_NMM_CORE 0`

	* 明确定义不构建的模型核心

`setenv WRF_DA_CORE 0`

	* 明确定义没有数据同化

`setenv NETCDF /usr/local/netcdf`（或者其他存放位置）

	* 所有WRF组件都需要链接此lib和include目录

`setenv OMP_NUM_THREADS n`（n是使用的核心数）

	* 如果您的系统上装有OpenMP，这样可以指定线程数

`setenv MP_STACK_SIZE 64000000`

	* 将OpenMP内存设置得很大
	
	* 但是，如果模型仍然崩溃，则可能是过度指定内存大小的问题。应将内存大小设置为足够大，但不能无限。
	
	* 在某些系统上，等效参数可以是KMP_STACKSIZE或OMP_STACKSIZE

`unlimit`

* 特别是在小型系统上时

<a id=Building_WRF></a>

## 编译WRF核心

WRF代码具有相当复杂的构建机制。它尝试确定您所使用的体系结构，然后为您提供选项以允许您选择首选的构建方法。例如，如果您使用的是Linux计算机，它将确定这是32位还是64位计算机，然后提示您使用所需的处理器（例如串行、共享内存或分布式内存）。您可以从构建机制的可用编译选项中进行选择。例如，如果您的系统上未安装PGI编译器，则不要选择PGI构建。

这个[网站](http://www2.mmm.ucar.edu/wrf/OnLineTutorial/compilation_tutorial.php )描述了构建WRF和WPS代码所需的步骤顺序（尽管这些说明是专门为tcsh和GNU编译器提供的）。

1. 从[此处](http://www2.mmm.ucar.edu/wrf/users/download/get_source.html )获得WRF V4的WRF的tar压缩文件（如果您不打算持续进行长时间的项目或者重复以前的工作时，请始终获取最新版本）

2. 解压缩文件

	`gzip -cd WRFV4.0.TAR.gz | tar -xf –`

	在某些系统上也可以使用`tar –xzf WRFV4.0.TAR.gz`

3. `cd WRF`

4. `./configure`

	**serial**表示单处理器
	
	**smpar**表示对称多处理/共享内存并行（OpenMP）——在大多数非IBM计算机上无法可靠运行
	
	**dmpar**表示分布式内存并行（MPI）
	
	**dm+sm**表示具有共享内存的分布式内存（例如，跨节点的MPI（在一个节点内具有OpenMP的节点））——通常比只有采用**dmpar**时有更好的性能
	
	第二个选项用于嵌套：0=不嵌套，1=标准静态嵌套，2=带有一组预定动作的嵌套，3=允许域跟随涡旋的嵌套（台风跟踪）
	
	**./configure**命令中可能包含的典型选项是标志**“-d”**（用于调试）。此选项删除优化，这在运行调试器（例如gdb或dbx）时非常有用
	
	为了进行边界检查和其他一些异常处理，可以选择调试标志**“-D”**。仅PGI、Intel和gfortran已设置为使用此选项。
	
5. `./compile em_real` (或`./WRF/test`中的任何目录名称)

6. `ls -ls main/*.exe`

	如果构建了real-data案例，则应该看到ndown.exe，real.exe和wrf.exe
	
	如果构建了ideal-data案例，应该会看到Ideal.exe和wrf.exe

WRF代码支持并行构建选项，该选项可通过并行make同时在单独的处理器上编译WRF目录中的单独源代码文件（尽管这些处理器需要共享内存）。并行构建选项的目的是能够加快构建可执行文件所需的时间。实际上，用户通常会看到大约2倍的提速，这是由于模块和USE关联而导致的代码中各种依赖项所施加的限制。要启用并行构建选项，用户需要设置环境变量J。在csh中，要使用两个处理器，请在`./compile`命令之前发出以下命令：

`setenv J “-j 2”`

用户可能希望仅使用单个处理器进行构建。在这种情况下：

`setenv J “-j 1”`

希望运行WRF化学代码的用户必须首先下载WRF模型的tar文件，然后将其解压缩。然后，将化学代码放到WRF目录（这是chem目录结构）中。合并tar文件中的源代码后，用户即可继续进行WRF化学构建。

<a id=Building_WPS></a>

## 编译WPS核心

**构建WPS要求已构建WRF。

**如果您打算使用Grib2数据，则需要zlib、png和jasper的其他库。请参阅[第3章](users_guide_chap3.md#How_to_Install)中的详细信息。

1. 从[此位置](http://www2.mmm.ucar.edu/wrf/users/download/get_source.html )获取WPS的tar压缩文件WPSV4.0.TAR.gz

2. 还可以从同一页面下载地理数据集。对于北美地区（NLCD）有新的数据集；对于某些北美城市，有高分辨率的城市数据集。

3. 解压源代码文件

	`gzip -cd WPSV4.0.TAR.gz | tar -xf -`

4. `cd WPS`

5. `./configure`

	选择一个选项
	
	通常，串行构建是进行初始测试的最佳选择。大多数大型域都可以使用单个处理器进行WPS
	
	WPS要求您针对适当的Grib解码进行构建。选择一个适合您将与ungrib程序一起使用的数据的选项（Grib2选项可用于Grib1或Grib2数据）
	
	如果选择Grib2选项，则必须预先准备和构建那些库（有关这些压缩库的位置，请参阅[WPS上的章节](users_guide_chap3.md#How_to_Install )）。并在configure.wps中使用变量COMPRESSION_LIBS和COMPRESSION_INC添加这些libraries和include文件的路径。同样，configure.wps文件中还包含构建的WRF目录的位置，需要对其进行修改。这就是WPS拾取所有必需的IO件以构建geogrid.exe和metgrid.exe文件的方式。

6. `./compile`

7. `ls -ls *.exe`

	您应该看到geogrid.exe、ungrib.exe和metgrid.exe（如果同时缺少geogrid.exe和metgrid.exe，则可能需要修复configure.wps文件中指向WRF的路径的位置；如果您 缺少ungrib.exe，请尝试使用仅Grib1的版本以进一步解决问题）

8. `ls -ls util/*.exe`
	
	您应该看到许多实用程序可执行文件：avg_tsfc.exe、calc_ecmwf_p.exe、g1print.exe、g2print.exe、height_ukmo.exe、mod_levs.exe、plotfmt.exe、plotgrids.exe和rd_intermediate.exe（需要NCAR Graphics的文件是plotfmt.exe和plotgrids.exe）
	
9. 如果缺少geogrid.exe和metgrid.exe可执行文件，则所构建的WRF目录结构的路径可能不正确（可在configure.wps文件中找到）

10. 如果缺少ungrib.exe，则Grib2库可能未正确链接或构建

11. 如果缺少plotfmt.exe或plotgrids.exe程序，则NCAR Graphics路径可能设置不正确