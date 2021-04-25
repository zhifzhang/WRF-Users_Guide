# 第7章 客观分析（OBSGRID）

## 目录

1. [简介](#Introduction)

2. [程序流程](#Program_Flow)

3. [观测数据来源](#Source_of_Obs)

4. [OBSGRID中使用的客观分析技术](#OA_tech)

5. [观测质量控制](#QC)

6. [附加观测](#Additional_Obs)

7. [地面FDDA选项](#Surface_FDDA)

8. [模型嵌套中的客观分析](#OA_Nests)

9. [如何运行OBSGRID](#How_to_run)

10. [输出文件](#Output)

11. [图形工具](#Plot_Utilities)

12. [观测格式](#Obs_Format)

13. [OBSGRID Namelist](#Namelist)

<a id=Introduction></a>

## 简介

气象模型中客观分析的目的是通过合并来自观测的信息以改进中尺度网格上的气象分析（`first guess`）。传统上，这些观测是来自地面和雷达探空仪报告的温度、湿度和风的“直接”观测。随着遥感技术的日趋成熟，研究人员和模型操作人员可以使用越来越多的“间接”观测。有效地将这些间接观察用于客观分析并非易事。通常用于间接观测的方法包括三维或四维变分技术（分别为“3DVAR”和“4DVAR”），这些技术也可以用于直接观测。

本章将讨论客观分析程序OBSGRID。有关变分技术（WRFDA）的讨论可见本用户指南第6章。

输入到OBSGRID的分析（`first guess`），是WPS软件包的METGRID部分的分析输出（有关WPS软件包的详细信息，请参见本用户指南的第3章）。

OBSGRID功能包括：

	* 选择Cressman-style或Multiquadric客观分析。
	
	* 为可疑的观测进行各种测试以筛选数据。
	
	* 可输入伪造数据。
	
	* 扩展网格：OBSGRID具有将输入模型区域缩减到输出范围的功能。利用此功能，您可以合并来自预期网格范围之外的数据，以改善边界附近的分析。若要使用此功能，用户在运行WPS时必须创建一个比最终目标区域更大的区域。

<a id=Program_Flow></a>

## 程序流程

OBSGRID在metgrid.exe之后直接运行，并使用来自metgrid.exe的`met_em*`输出文件作为输入文件。OBSGRID还需要其他观察数据（A）作为输入文件。这些观测文件的格式在本章的[观测格式](#Obs_Format)部分中进行了描述。

![Program_Flow](images/chap7_Program_Flow.png)

客观分析程序的输出文件可用于：

	- 提供用于初始条件和边界条件的字段（1）。请注意，`metoa_em*`文件的格式与来自metgrid.exe的`met_em*`文件的格式相同。唯一的区别是这些文件中的字段合并了观测信息。
	
	- 提供表面场以进行表面分析微调FDDA（2）。注意，将wrfsfdda文件用作WRF的输入文件时，还建议使用3-D fdda文件（wrffdda（5）–运行real.exe时创建的可选输出）作为WRF的输入文件。
	
	- 提供用于观测微调的数据（3）。注意：从OBSGRID的3.1.1版本开始，该文件可以由观测微调代码直接读取，不再需要通过其他perl脚本来传递。
	
	- 提供ASCII和netCDF输出（4）。这些文件包含有关使用的观测和分配的质量控制标志的信息。这些文件中的信息也可以使用提供的绘图工具进行绘图。

<a id=Source_of_Obs></a>

## 观测数据来源

OBSGRID读取用户以格式化的ASCII文本文件提供的观测数据。这使用户可以调整自己的数据以用作OBSGRID程序的输入。此格式（[`wrf_obs/little_r`格式](http://www2.mmm.ucar.edu/mm5/mm5v3/data/how_to_get_rawdata.html )）与MM5客观分析程序LITTLE_R中使用的格式相同（因此而得名）。

可以使用程序将NMC ON29和NCEP BUFR格式的文件（参见下文）转换为`wrf_obs/little_r`格式。用户负责将他们希望提供给OBSGRID的其他观测结果转换为这种格式。`utils/`目录中有一个用户提供（即不受官方支持）的程序，用于将观测文件从GTS转换为`wrf_obs/little_r`格式。

NCEP全球地面和高空观测业务数据子集，由NCAR的数据支持科（DSS）存档。

	- NMC ON29格式的高空数据（从1970年代初到2000年初） http://rda.ucar.edu/datasets/ds353.4/  

	- NMC ON29格式的地面数据（从1970年代初到2000年初） http://rda.ucar.edu/datasets/ds464.0/ 

	- NCEP BUFR格式的高空数据（从1999年至今） http://rda.ucar.edu/datasets/ds351.0/ 

	- NCEP BUFR格式的地面数据（从1999年至今） http://rda.ucar.edu/datasets/ds461.0/  

较新的数据（ds351.0和ds461.0）也提供little_r格式。 在NCAR以外，可以从Web上下载此数据，而NCAR超级计算机用户可以在NCAR/glade系统上使用该数据。该数据被分为6小时一次的窗口，这些窗口通常太大而无法在OBSGRID中使用。要将其重新排序为3小时一次的窗口，请执行以下操作：

	- 获取little_r 6小时数据
	
		- 非NCAR超级计算机用户，直接从上述网站获取数据。将所有地面数据和高空数据合并（使用Unix的“cat”命令）到一个名为`rda_obs`的大文件中。
		
		- NCAR超级计算机用户，使用`util/get_rda_data.csh`脚本获取数据并创建`rda_obs`文件。您需要编辑此脚本以提供您感兴趣的日期范围。

	- 编译Fortran程序`util/get_rda_data.f`。将`rda_obs`文件放在顶级OBSGRID目录中。运行`util/get_rda_data.exe`可执行文件。该可执行文件将使用`namelist.oa`中的日期范围，创建3小时一次的`OBS:<date>`文件，以供OBSGRID使用。
	
NMC Office Note 29可以在万维网上的许多地方找到，包括： http://www.emc.ncep.noaa.gov/mmb/data_processing/on29.htm 

获取little_r观测数据的另一种方法是从气象同化数据摄取系统（[MADIS](https://madis.noaa.gov/ )）下载，并使用NCAR提供的[MADIS2LITTLER工具](http://www2.mmm.ucar.edu/wrf/users/wrfda/download/madis.html )将其转换为little_r格式。请注意，要允许OBSGRID正确处理单层地面以上观测数据，必须修改MADIS2LITTLER以将这样的观测标记为探测（在`module_output.F`中，将`write_littler_onelvl`子例程修改设置为`is_sound = .TRUE.`）。

<a id=OA_tech></a>

## OBSGRID中使用的客观分析技术

### Cressman方案

OBSGRID中使用的四种客观分析技术中的三种是基于Cressman方案的，该方案通过多次连续扫描将第一猜测场向邻近的观测值微调。

标准Cressman方案为每个观测站分配一个圆形的影响半径R。每个网格点P处的第一猜测场，是通过考虑所有影响P的观测站来调整的。

计算第一猜测场和观测值之间的差异，并将这些差异值的距离加权平均值添加到P处的第一猜测值。一旦调整了所有网格点，就使用调整后的字段作为另一个调整周期的第一猜测场。随后的逐个调整均使用较小的影响半径。

![Cressman_Scheme](images/chap7_Cressman_Scheme.png)

### Ellipse方案

在分析压力层上的风和相对湿度（因风而强烈变形的场）时，标准Cressman方案的圆被沿着风向方向拉长成椭圆形。风越强，椭圆的偏心率就越大。该方案在低风速条件下就简化为圆形Cressman方案。

![Ellipse_Scheme](images/chap7_Ellipse_Scheme.png)

### Banana方案

在分析压力层上的风和相对湿度时，标准Cressman方案的圆在流动方向上拉长，并沿流线弯曲，变成香蕉形状。在直线流动条件下，该方案简化为Ellipse方案，在低风速条件下，该方案简化为标准Cressman方案。

![Banana_Scheme](images/chap7_Banana_Scheme.png)
 
### Multiquadric方案

Multiquadric方案使用双曲面径向基函数执行客观分析。Multiquadric技术的详细信息可见Nuss and Titley, 1994: "Use of multiquadric interpolation for meteorological objective analysis." Mon . Wea . Rev ., 122, 1611-1631。请谨慎使用此方案，因为它在只有少量观测数据的区域中可能会产生一些奇怪的结果。

<a id=QC></a>

## 观测质量控制

OBSGRID的重要组成部分是筛选不佳的观测结果。在OBSGRID中，许多这些质量控制检查是可选的。

### 独立报告的质量控制

	- 总误差检查（相同值、压力随高度降低等）
	
	- 去除温度和风廓线中的尖峰
	
	- 调整温度廓线以去除超绝热层
	
	- 不能与其他报告或第一猜测场进行比较

### ERRMAX测试

ERRMAX质量控制检查是可选的，但强烈建议使用。

	- 限制对数据删除的用户控制。用户可以设置阈值，这些阈值会改变错误检查的容忍度
	
	- 将观察结果与第一猜测场进行比较

	- 如果差值（观测值-第一猜测场）超过某个阈值，则该观测值将被丢弃
	
	- 按字段、层和一天中的时间设置不同的阈值
	
	- 与良好的第一猜测场配合良好

### Buddy测试

Buddy检查是可选的，但强烈建议使用。

	- 限制对数据删除的用户控制。用户可以设置权重因子，这会改变错误检查的容忍度
	
	- 将观测结果与第一猜测场和附近的观测结果进行比较

	- 如果某个观测点的差异值（观测值-第一猜测场）与相邻观测点的差异值的距离加权平均值相差很大，则丢弃该观测值
	
	- 在数据密度好的区域中效果很好

<a id=Additional_Obs></a>

## 附加观测

在客观分析阶段，输入附加观测值或修改现有（和错误的）观测值可能是有用的工具。

在OBSGRID中，以与标准观测相同的方式（以相同的`wrf_obs/little_r`格式）向程序提供附加观测。附加观测必须与其他观测保存在同一文件中。现有（错误）的观测值可以轻松修改，因为观测值输入格式为ASCII文本。将一个观测报告标识为“bogus”仅意味着它被认为是良好的数据，但未对该报告执行任何质量控制检查。

<a id=Surface_FDDA></a>

## 地面FDDA选项

地面FDDA选项仅针对地面创建其他分析文件，通常两次分析之间的时间间隔比完整的高空分析要小（即更频繁）。这些地面分析文件可供以后在WRF中配合地面分析微调选项使用。

LAGTEM选项控制如何为地面分析文件创建第一猜测场。通常，以十二小时或六小时的时间间隔提供地面和高空第一猜测（分析时间），而地面分析间隔可以为3小时（10800秒）。因此，在分析时，将使用可用的地面第一猜测。如果将LAGTEM设置为`.FALSE.`，其他时间的地面第一猜测将从分析时间的第一猜测中临时插值。如果将LAGTEM设置为`.TRUE.`，则其他时间的地面第一猜测是前一次的客观分析。

<a id=OA_Nests></a>

## 模型嵌套中的客观分析

OBSGRID具有对嵌套执行客观分析的功能。这是通过单独的OBSGRID流程手动完成的，该流程在针对特定嵌套的met_em_d0x文件上执行。但是，通常不需要这样做，它会使用户的事务复杂化，并可能在预测中引入错误。另一方面，用户可用的额外信息，或客观分析可能在嵌套中提供的额外详细信息，使对嵌套的客观分析成为一个不错的选择。

对嵌套进行客观分析的主要原因是您具有比粗略域分辨率更大水平分辨率的观测数据。在某些情况下，嵌套上的地形也可以更好地利用地面观测值（即模型地形更好地与观测值的真实地形高度匹配）。

通过对嵌套进行客观分析而引入的主要问题是，粗糙域和嵌套之间的初始条件不一致。落在嵌套之外的观测值将用于分析粗略域，而在嵌套分析中将其丢弃。在嵌套边界处使用不同的观测值，可以得到非常不同的分析结果。

<a id=How_to_run></a>

## 如何运行OBSGRID

### 获取源代码

可以从[本网址](https://github.com/wrf-model/OBSGRID )或[本网址](https://www2.mmm.ucar.edu/wrf/users/download/get_sources_pproc_util.html )获取源代码。将tar文件解压缩（`gunzip OBSGRID.TAR.gz`，`tar -xf OBSGRID.TAR`）后，将生成一个`OBSGRID/`目录。

`cd OBSGRID`

### 生成可执行文件

构建WRF模型所需的唯一库是netCDF。用户可以在[UNIDATA主页](http://www.unidata.ucar.edu/software/netcdf/ )上找到源代码、预编译的二进制文件和文档。

要成功编译实用程序`plot_level.exe`和`plot_sounding.exe`，需要在系统上安装NCAR Graphics。这些例程对于运行OBSGRID不是必需的，但是对于显示观测值很有用。自从3.7.0版本后，提供了NCL脚本，因此不再需要这两个实用程序来绘制数据。

要配置，请键入：

`./configure`

选择配置选项之一，然后进行编译。

`./compile`

如果成功，将创建可执行文件`obsgrid.exe`。如果安装了NCAR Graphics，将创建可执行文件`plot_level.exe`和`plot_sounding.exe`。

### 准备观测文件

需要用户准备观测文件。一些数据可以从NCAR的RDA网站上获得。从1970年代初期开始的数据采用ON29格式，而1999年至今的数据则采用NCEP BUFR格式。提供了使用这些数据集的帮助。有关更多信息，请参阅本用户指南的[观测数据来源](#Source_of_Obs)。

还提供了一个用于重新格式化GTS流中的观测值的程序（不受官方支持）。可以在`OBSGRID/util`中找到它，称为`gts_cleaner.f`。该代码希望每个分析时间找到一个观测输入文件。每个文件都应包含地面和高空数据（如果有）。

### 根据您的具体情况编辑namelist

最经常更改的最关键信息是开始日期、结束日期和文件名。

应特别注意文件名的设置。观测文件的名称错误可能不会被发现，因为OBSGRID会很处理错误的文件，并且如果（错误指定的）文件中的特定时间段内没有数据，OBSGRID会为您提供无观测值的分析结果。

### 运行程序

通过调用以下命令运行程序：

`./obsgrid.exe >& obsgrid.out`

检查`obsgrid.out`文件以获取信息和运行错误。

### 检查输出文件

检查`obsgrid.out`文件中的错误消息或警告消息。该程序应该已经创建了名为`metoa_em*`的文件。也可能会创建其他输出文件，其中包含有关发现、使用和丢弃的观测数据的信息。

要检查的重要事项包括为您进行客观分析而发现的观测数，以及在各个层上使用的观测数。这可以提醒您在指定观测文件或时间间隔时可能出现的问题。此信息包含在打印输出文件中。

您可能还想尝试使用几个简单的绘图实用程序，详见下述。

还有许多其他输出文件，您可能会发现它们很有用，详见下述。

<a id=Output></a>

## 输出文件

The OBSGRID program generates some ASCII/netCDF files to detail the actions taken on observations through a time cycle of the program. In support of users wishing to plot the observations used for each variable (at each level, at each time), a file is created with this information. Primarily, the ASCII/netCDF files are for consumption by the developers for diagnostic purposes. The main output of the OBSGRID program is the gridded, pressure-level data set to be passed to the real.exe program (files metoa_em*).
In each of the files listed below, the text ".dn.YYYY-MM-DD_HH:mm:ss.tttt" allows each time period that is processed by OBSGRID to output a separate file. The only unusual information in the date string is the final four letters "tttt" which is the decimal time to ten thousandths of a second. These files will be dependent on the domain being processed.

metoa_em*
These are the final analysis files at surface and pressure levels. Generating this file is the primary goal of running OBSGRID.
These files can now be used in place of the met_em* files from WPS to generate initial and boundary conditions for WRF. To use these files when running real.exe you can do one of two things:
1.	Rename or link the metoa_em* files back to met_em*. This way real.exe will read the files automatically. 
2.	Use the auxinput1_inname namelist option in WRF’s namelist.input file to overwrite the default filename real.exe uses. To do this, add the following to the &time_control section of the WRF namelist.input file before running real.exe (use the exact syntax as below – do not substitute the <domain> and <date> for actual numbers):

auxinput1_inname = "metoa_em.d<domain>.<date>"

wrfsfdda_dn
Use of the surface FDDA option in OBSGRID creates a file called wrfsfdda_dn. This file contains the surface analyses at INTF4D intervals, analyses of T, TH, U, V, RH, QV, PSFC, PMSL, and a count of observations within 250 km of each grid point.
Due to the input requirements of the WRF model, data at the current time (_OLD) and data for the next time (_NEW) are supplied at each time interval. Due to this requirement, users must take care to specify the same interval in the WRF fdda section for surface nudging as the interval used in OBSGRID to create the wrfsfdda_dn file.  This also means that the user may need to have data available for OBSGRID to create a surface analysis beyond the last analysis actually used by WRF surface analysis nudging.  With a positive value for the length of rampdown, even though the _OLD field at the beginning of the rampdown will be nudged throughout the rampdown, WRF still requires a _NEW field at the beginning of the rampdown period.

OBS_DOMAINdxx
These files can be used in WRF for observational nudging. The format of this file is slightly different from the standard wrf_obs / little_r format. See the Observation Nudging User's Guide or Chapter 5 of this User’s Guide for details on observational nudging. 
The “d” in the file name represents the domain number. The “xx” is just a sequential number. 
These files contain a list of all of the observations available for use by the OBSGRID program.
•	The observations have been sorted and the duplicates have been removed. 
•	Observations outside of the analysis region have been removed. 
•	Observations with no information have been removed. 
•	All reports for each separate location (different levels, but at the same time) have been combined to form a single report. 
•	Data that has had the "discard" flag internally set (data which will not be sent to the quality control or objective analysis portions of the code) are not listed in this output. 
•	The data have gone through an expensive test to determine if the report is within the analysis region, and the data have been given various quality control flags. Unless a blatant error in the data is detected (such as a negative sea-level pressure), the observation data are not typically modified, but only assigned quality control flags.
•	Data with qc flags higher than a specified value (user controlled, via the namelist), will be set to missing data.
The WRF observational nudging code requires that all observational data are available in a single file called OBS_DOMAINd01 (where d is the domain number), whereas OBSGRID creates one file per time. Therefore, to use these files in WRF, they should first be concatenated to a single file. A script (run_cat_obs_files.csh) is provided for this purpose. By running this script, the original OBS_DOMAINd01 files will be moved to OBS_DOMAINd01_sav, and a new OBS_DOMAINd01 file (containing all the observations for all times) will be created. This new file can be used directly in the WRF observational nudging code.

qc_obs_raw.dn.YYYY-MM-DD_HH:mm:ss.tttt(.nc)
This file contains a listing of all of the observations available for use by the OBSGRID program.
•	The observations have been sorted and the duplicates have been removed. 
•	Observations outside of the analysis region have been removed. 
•	Observations with no information have been removed. 
•	All reports for each separate location (different levels, but at the same time) have been combined to form a single report. 
•	Data that has had the "discard" flag internally set (data which will not be sent to the quality control or objective analysis portions of the code) are not listed in this output. 
•	The data have gone through an expensive test to determine if the report is within the analysis region, and the data have been given various quality control flags. Unless a blatant error in the data is detected (such as a negative sea-level pressure), the observation data are not typically modified, but only assigned quality control flags.
•	Two files are available, both containing identical information. One is the older ASCII format, while the other is in netCDF format. 
•	The data in the ASCII file can be used as input to the plotting utility plot_sounding.exe 
•	The netCDF file can be used to plot both station data (util/station.ncl) and sounding data (util/sounding.ncl). This is available since version 3.7 and is the recommended option. 

qc_obs_used.dn.YYYY-MM-DD_HH:mm:ss.tttt(.nc)
These files are similar to the above “raw” files, and can be used in the same way. But in this case it contains the data used by the OBSGRID program, which are also the data saved to the OBS_DOMAINdxx files.
qc_obs_used_earth_relative.dn.YYYY-MM-DD_HH:mm:ss.tttt(.nc)
These files are identical to the above "qc_obs_used" files except that the winds are in an earth-relative framework rather than a model-relative framework.  The non-netCDF version of these files can be used as input for the Model Evaluation Tools (MET; http://www.dtcenter.org/met/users/).

plotobs_out.dn.YYYY-MM-DD_HH:mm:ss.tttt
This file lists data by variable and by level, where each observation that has gone into the objective analysis is grouped with all of the associated observations for plotting or some other diagnostic purpose. The first line of this file is the necessary FORTRAN format required to input the data. There are titles over the data columns to aid in the information identification. Below are a few lines from a typical file. This data can be used as input to the plotting utility plot_level.exe. But since version 3.7, it is recommended to use the station.ncl script that uses the data in the new netCDF data files. 

( 3x,a8,3x,i6,3x,i5,3x,a8,3x,2(g13.6,3x),2(f7.2,3x),i7 ) 
Number of Observations 00001214 
Variable Press  Obs    Station Obs        Obs-1st   X         Y         QC 
Name     Level  Number ID      Value      Guess     Location  Location  Value 
U        1001   1      CYYT    6.39806    4.67690   161.51    122.96    0 
U        1001   2      CWRA    2.04794    0.891641  162.04    120.03    0 
U        1001   3      CWVA    1.30433   -1.80660   159.54    125.52    0 
U        1001   4      CWAR    1.20569    1.07567   159.53    121.07    0 
U        1001   5      CYQX    0.470500  -2.10306   156.58    125.17    0 
U        1001   6      CWDO    0.789376  -3.03728   155.34    127.02    0 
U        1001   7      CWDS    0.846182   2.14755   157.37    118.95    0 

<a id=Plot_Utilities></a>

## 图形工具

The OBSGRID package provides two utility programs for plotting observations. These programs are called plot_soundings.exe and plot_levels.exe. These optional programs use NCAR Graphics to build, which is often problematic. Two new NCL scripts are provided instead, sounding.ncl and station.ncl. Using these as opposed to the Fortran code are recommended. 
sounding.ncl / plot_soundings.exe
The script util/sounding.ncl plots soundings. This script generates soundings from the netCDF files qc_obs_raw.dn.YYYY-MM-DD_HH:mm:ss.tttt.nc and qc_obs_used.dn.YYYY-MM-DD_HH:mm:ss.tttt.nc. Only data that are on the requested analysis levels are processed. 
By default the script will plot the data from all the “qc_obs_used” files in the directory. This can be customized through the use of command line setting. For example:
	ncl ./util/sounding.ncl 'qcOBS="raw"'
		will plot data from the “qc_obs_raw” files
	ncl util/sounding.ncl YYYY=2010 MM=6
		will plot data from the “qc_obs_used” files for June 2010
Available command line options are:
qcOBS	Dataset to use. Options are “raw” or “used”. Default is “used”
YYYY	Integer year to plot. Default is all available years.
MM	Integer month to plot. Default is all available months.
DD	Integer day to plot. Default is all available days.
HH	Integer hour to plot. Default is all available hours.
outTYPE	Output type. Default is plotting to the screen, i.e., “x11”. Other options are “pdf” or “ps”.
The script creates the following output files(s):
qc_obs_<qcOBS>.sounding.<date>.<outTYPE> for instance:
qc_obs_used.sounding.2010-03-06_09.pdf 

The older program plot_soundings.exe also plots soundings. This program generates soundings from the qc_obs_raw.dn.YYYY-MM-DD_HH:mm:ss.tttt and qc_obs_used.dn.YYYY-MM-DD_HH:mm:ss.tttt data files. Only data that are on the requested analysis levels are processed. The program uses information from &record1, &record2 and &plot_sounding in the namelist.oa file to generate the required output. The program creates output file(s): sounding_<file_type>_<date>.cgm

plot_level.exe
The script util/station.ncl creates station plots for each analysis level. These plots contain both observations that have passed all QC tests and observations that have failed the QC tests. Observations that have failed the QC tests are plotted in various colors according to which test failed. This script generates soundings from the netCDF files qc_obs_raw.dn.YYYY-MM-DD_HH:mm:ss.tttt.nc and qc_obs_used.dn.YYYY-MM-DD_HH:mm:ss.tttt.nc.
By default the script will plot the data from all the “qc_obs_used” files in the directory. This can be customized through the use of command line setting. For example:
	ncl ./util/station.ncl 'qcOBS="raw"'
		will plot data from the “qc_obs_raw” files
	ncl util/station.ncl YYYY=2010 MM=6
		will plot data from the “qc_obs_used” files for June 2010
Available command line options are:
qcOBS	Dataset to use. Options are “raw” or “used”. Default is “used”
YYYY	Integer year to plot. Default is all available years.
MM	Integer month to plot. Default is all available months.
DD	Integer day to plot. Default is all available days.
HH	Integer hour to plot. Default is all available hours.
outTYPE	Output type. Default is plotting to the screen, i.e., “x11”. Other options are “pdf” or “ps”.
The script creates the following output files(s):
qc_obs_<qcOBS>.station.<date>.<outTYPE> for instance:
qc_obs_used.station.2010-03-06_09.pdf 
The older program plot_level.exe creates station plots for each analysis level. These plots contain both observations that have passed all QC tests and observations that have failed the QC tests. Observations that have failed the QC tests are plotted in various colors according to which test failed. The program uses information from &record1 and &record2 in the namelist.oa file to generate plots from the observations in the file plotobs_out.dn.YYYY-MM-DD_HH:mm:ss.tttt. The program creates the file(s): levels_<date>.cgm.

<a id=Obs_Format></a>

## 观测格式

To make the best use of the OBSGRID program, it is important for users to understand the wrf_obs/little_r Observations Format.
Observations are conceptually organized in terms of reports. A report consists of a single observation or set of observations associated with a single latitude/longitude coordinate.
Examples
•	a surface station report including observations of temperature, pressure, humidity, and winds.
•	an upper-air station's sounding report with temperature, humidity, and wind observations at many height or pressure levels.
•	an aircraft report of temperature at a specific lat/lon/height.
•	a satellite-derived wind observation at a specific lat/lon/height.
Each report in the wrf_obs/little_r Observations Format consists of at least four records:
•	A report header record 
•	one or more data records 
•	an end data record 
•	an end report record .
The report header record is a 600-character-long record (much of which is unused and needs only dummy values) that contains certain information about the station and the report as a whole (location, station id, station type, station elevation, etc.). The report header record is described fully in the following table. Shaded items in the table are unused:
Report header format
Variable	Fortran I/O Format	Description
latitude	F20.5	station latitude (north positive)
longitude	F20.5	station longitude (east positive)
id	A40	ID of station
name	A40	Name of station
platform	A40	Description of the measurement device
source	A40	GTS, NCAR/ADP, BOGUS, etc.
elevation	F20.5	station elevation (m)
num_vld_fld	I10	Number of valid fields in the report
num_error	I10	Number of errors encountered during the decoding of this observation
num_warning	I10	Number of warnings encountered during decoding of this observation.
seq_num	I10	Sequence number of this observation
num_dups	I10	Number of duplicates found for this observation
is_sound	L10	T/F Above-surface or surface (i.e., all non-surface observations should use T, even above-surface single-level obs)
bogus	L10	T/F bogus report or normal one
discard	L10	T/F Duplicate and discarded (or merged) report.
sut	I10	Seconds since 0000 UTC 1 January 1970
julian	I10	Day of the year
date_char	A20	YYYYMMDDHHmmss
slp, qc	F13.5, I7	Sea-level pressure (Pa) and a QC flag
ref_pres, qc	F13.5, I7	Reference pressure level (for thickness) (Pa) and a QC flag
ground_t, qc	F13.5, I7	Ground Temperature (T) and QC flag
sst, qc	F13.5, I7	Sea-Surface Temperature (K) and QC
psfc, qc	F13.5, I7	Surface pressure (Pa) and QC
precip, qc	F13.5, I7	Precipitation Accumulation and QC
t_max, qc	F13.5, I7	Daily maximum T (K) and QC
t_min, qc	F13.5, I7	Daily minimum T (K) and QC
t_min_night, qc	F13.5, I7	Overnight minimum T (K) and QC
p_tend03, qc	F13.5, I7	3-hour pressure change (Pa) and QC
p_tend24, qc	F13.5, I7	24-hour pressure change (Pa) and QC
cloud_cvr, qc	F13.5, I7	Total cloud cover (oktas) and QC
ceiling, qc	F13.5, I7	Height (m) of cloud base and QC
Following the report header record are the data records. These data records contain the observations of pressure, height, temperature, dewpoint, wind speed, and wind direction. There are a number of other fields in the data record that are not used on input. Each data record contains data for a single level of the report. For report types that have multiple levels (e.g., upper-air station sounding reports), each pressure or height level has its own data record. For report types with a single level (such as surface station reports or a satellite wind observation), the report will have a single data record. The data record contents and format are summarized in the following table
Format of data records
Variable	Fortran I/O Format	Description
pressure, qc	F13.5, I7	Pressure (Pa) of observation, and QC
height, qc	F13.5, I7	Height (m MSL) of observation, and QC
temperature, qc	F13.5, I7	Temperature (K) and QC
dew_point, qc	F13.5, I7	Dewpoint (K) and QC
speed, qc	F13.5, I7	Wind speed (m/s) and QC
direction, qc	F13.5, I7	Wind direction (degrees) and QC
u, qc	F13.5, I7	u component of wind (m/s), and QC
v, qc	F13.5, I7	v component of wind (m/s), and QC
rh, qc	F13.5, I7	Relative Humidity (%) and QC
thickness, qc	F13.5, I7	Thickness (m), and QC
The end data record is simply a data record with pressure and height fields both set to -777777.
After all the data records and the end data record, an end report record must appear. The end report record is simply three integers, which really aren't all that important.

Format of end_report records
Variable	Fortran I/O Format	Description
num_vld_fld	I7	Number of valid fields in the report
num_error	I7	Number of errors encountered during the decoding of the report
num_warning	I7	Number of warnings encountered during the decoding the report
QCFlags
In the observation files, most of the meteorological data fields also have space for an additional integer quality-control flag. The quality-control values are of the form 2n, where n takes on positive integer values. This allows the various quality control flags to be additive, yet permits the decomposition of the total sum into constituent components. Following are the current quality control flags that are applied to observations:
pressure interpolated from first-guess height      = 2 **  1 =      2 pressure int. from std. atmos. and 1st-guess height= 2 **  3 =      8
temperature and dew point both = 0                 = 2 **  4 =     16
wind speed and direction both = 0                  = 2 **  5 =     32
wind speed negative                                = 2 **  6 =     64
wind direction < 0 or > 360                        = 2 **  7 =    128
level vertically interpolated                      = 2 **  8 =    256
value vertically extrapolated from single level    = 2 **  9 =    512
sign of temperature reversed                       = 2 ** 10 =   1024
superadiabatic level detected                      = 2 ** 11 =   2048
vertical spike in wind speed or direction          = 2 ** 12 =   4096
convective adjustment applied to temperature field = 2 ** 13 =   8192 
no neighboring observations for buddy check        = 2 ** 14 =  16384
---------------------------------------------------------------------- 
data outside normal analysis time and not QC-ed    = 2 ** 15 =  32768
----------------------------------------------------------------------
fails error maximum test                           = 2 ** 16 =  65536
fails buddy test                                   = 2 ** 17 = 131072 
observation outside of domain detected by QC       = 2 ** 18 = 262144

<a id=Namelist></a>

## OBSGRID Namelist

The OBSGRID namelist file is called "namelist.oa", and must be in the directory from which OBSGRID is run. The namelist consists of nine namelist records, named "record1" through "record9", each having a loosely related area of content. Each namelist record, which extends over several lines in the namelist.oa file, begins with "&record<#>" (where <#> is the namelist record number) and ends with a slash "/".
The namelist record &plot_sounding is only used by the corresponding utility.
Namelist record1
The data in namelist record1 define the analysis times to process:
Namelist Variable	Value	Description
start_year	2000	4-digit year of the starting time to process
start_month	01	2-digit month of the starting time to process
start_day	24	2-digit day of the starting time to process
start_hour	12	2-digit hour of the starting time to process
end_year	2000	4-digit year of the ending time to process
end_month	01	2-digit month of the ending time to process
end_day	25	2-digit day of the ending time to process
end_hour	12	2-digit hour of the ending time to process
interval	21600	Time interval (s) between consecutive times to process
Namelist record2
The data in record2 define the model grid and names of the input files: 
Namelist Variable	Value	Description
grid_id	1	ID of domain to process 
obs_filename	CHARACTER	Root file name (may include directory information) of the observational files. All input files must have the format obs_filename:<YYYY-MM-DD_HH>. 
One file required for each time period.
If a wrfsfdda is being created, then similar input data files are required for each surface fdda time.
remove_data_above_qc_flag	200000	Data with qc flags higher than this will not be output to the OBS_DOMAINdxx files. Default is to output all data. Use 65536 to remove data that failed the buddy and error max tests. To also exclude data outside analysis times that could not be QC-ed use 32768 (recommended).
This does not affect the data used in the OA process.
remove_unverified_data	.FALSE.	By setting this parameter to .TRUE. (recommended) any observations that could not be QC'd due to having a pressure insufficiently close to an analysis level will be removed from the OBS_DOMAINdxx files.  Obs QC'd by adjusting them to a nearby analysis level or by comparing them to an analysis level within a user-specified tolerance will be included in the OBS_DOMAINdxx files.  See use_p_tolerance_one_lev in &record4.
trim_domain	.FALSE.	Set to .TRUE. if this domain must be cut down on output
trim_value	5	Value by which the domain will be cut down in each direction 
The met_em* files which are being processed must be available in the OBSGRID/ directory.
The obs_filename and interval settings can get confusing, and deserve some additional explanation. Use of the obs_filename files is related to the times and time interval set in namelist &record1, and to the F4D options set in namelist &record8. The obs_filename files are used for the analyses of the full 3D dataset, both at upper levels and the surface. They are also used when F4D=.TRUE.; that is, if surface analyses are being created for surface FDDA nudging. The obs_filename files should contain all observations (upper-air and surface) to be used for a particular analysis at a particular time. 
Ideally there should be an obs_filename for each time period for which an objective analysis is desired. Time periods are processed sequentially from the starting date to the ending date by the time interval, all specified in namelist &record1. All observational files must have a date associated with them. If a file is not found, the code will process as if this file contains zero observations, and then continue to the next time period. 
If the F4D option is selected, the obs_filename files are similarly processed for surface analyses, this time with the time interval as specified by INTF4D.
If a user wishes to include observations from outside the model domain of interest, geogrid.exe (WPS) needs to be run over a slightly larger domain than the domain of interest. Setting trim_domain to .TRUE. will cut all 4 directions of the input domain down by the number of grid points set in trim_value. 
In the example below, the domain of interest is the inner white domain with a total of 100x100 grid points. geogrid.exe has been run for the outer domain (110x110 grid points). By setting the trim_value to 5, the output domain will be trimmed by 5 grid points in each direction, resulting in the white 100x100 grid point domain.  
 

Namelist record3
The data in the &record3 concern space allocated within the program for observations. These are values that should not frequently need to be modified:
Namelist Variable	Value	Description
max_number_of_obs	10000	Anticipated maximum number of reports per time period
fatal_if_exceed_max_obs	.TRUE.	T/F flag allows the user to decide the severity of not having enough space to store all of the available observation
Namelist record4 
The data in &record4 set the quality control options. There are four specific tests that may be activated by the user: An error max test; a buddy test; removal of spike, and; the removal of super-adiabatic lapse rates. For some of these tests, the user has control over the tolerances, as well. 
Namelist Variable	Value	Description
qc_psfc	.FALSE.	Execute error max and buddy check tests for surface pressure observations (temporarily converted to sea level pressure to run QC)
Error Max Test: For this test there is a threshold for each variable. These values are scaled for time of day, surface characteristics and vertical level.
qc_test_error_max	.TRUE.	Check the difference between the first-guess and the observation
max_error_t	10	Maximum allowable temperature difference (K)
max_error_uv	13 	Maximum allowable horizontal wind component difference (m/s)
max_error_z	8 	Not used
max_error_rh	50 	Maximum allowable relative humidity difference (%)
max_error_p	600 	Maximum allowable sea-level pressure difference (Pa
max_error_dewpoint	20	Maximum allowable dewpoint difference (K)
Buddy Check Test: For this test there is a threshold for each variable. These values are similar to standard deviations.
qc_test_buddy	.TRUE.	Check the difference between a single observation and neighboring observations
max_buddy_t	8	Maximum allowable temperature difference (K)
max_buddy_uv	8	Maximum allowable horizontal wind component difference (m/s)
max_buddy_z	8	Not used
max_buddy_rh	40	Maximum allowable relative humidity difference (%)
max_buddy_p	800	Maximum allowable sea-level pressure difference (Pa)
max_buddy_dewpoint	20	Maximum allowable dewpoint difference (K)
buddy_weight	1.0	Value by which the buddy thresholds are scaled
Spike removal
qc_test_vert_consistency	.FALSE.	Check for vertical spikes in temperature, dew point, wind speed and wind direction
Removal of super-adiabatic lapse rates
qc_test_convective_adj	.FALSE.	Remove any super-adiabatic lapse rate in a sounding by conservation of dry static energy
For satellite and aircraft observations, data are often horizontally spaced with only a single vertical level. The following entries determine how such data are dealt with and are described in more detail below the table.
use_p_tolerance_one_lev	.FALSE.	Should single-level above-surface observations be directly QC'd against nearby levels (.TRUE.) or extended to nearby levels (.FALSE.)
max_p_tolerance_one_lev_qc	700	Pressure tolerance within which QC can be applied directly (Pa)
max_p_extend_t	1300	Pressure difference (Pa) through which a single temperature report may be extended
max_p_extend_w	1300	Pressure difference (Pa) through which a single wind report may be extended
Dewpoint quality control: 
Note that the dewpoint error max check and buddy check are using the same moisture field as the relative humidity checks.  The dewpoint checks are to allow for an additional level of quality control on the moisture fields and may be helpful for dry observations where RH differences may be small but dewpoint differences are much larger.  The maximum dewpoint thresholds are scaled based on the observed dewpoint to increase the threshold for dry conditions where larger dewpoint variations are expected.  If the user does not wish to use dewpoint error checks, simply set the thresholds to very large values.

Quality control of single-level above-surface observations:

Option 1:  use_p_tolerance_one_lev = .FALSE.:
For single-level above-surface observations marked as 'FM-88 SATOB' or 'FM-97 AIREP', the observations are adjusted to the nearest pressure level.  If the observation's pressure is within max_p_extend_t Pa of the nearest first-guess level, the temperature of the observation is adjusted to the first-guess level using a standard lapse rate, otherwise the temperature is marked as missing.  If the observation’s pressure is within max_p_extend_w Pa of the nearest first-guess level, the winds are used without adjustment.  The dewpoint is marked as missing regardless of the pressure of the observation. The pressure of the observation is changed to be the pressure of the pressure level against which it is being quality controlled.
If a single-level above-surface observation is marked as anything other than ‘FM-88 SATOB’ or ‘FM-97 AIREP’, it appears that it will not be quality controlled unless its pressure happens to exactly match one of the pressure levels in the first guess field.  Note that max_p_tolerance_one_lev_qc is ignored if use_p_tolerance_one_lev = .FALSE.
Option 2: use_p_tolerance_one_lev = .TRUE.:
For all single-level above-surface observations, the observations will be quality controlled as long as the closest first-guess field is within max_p_tolerance_one_lev_qc Pa of the observation.  In order to allow all single-level above-surface observations to be close enough to a first-guess pressure level that quality control directly comparing the closest pressure level to the observation is valid, the user may need to interpolate the first guess to additional pressure levels prior to ingestion into OBSGRID.  OBSGRID will print out the pressure ranges for which error max quality control is not available (i.e., the pressures for which single-level above-surface observations will not be quality controlled).  See max_p_tolerance_one_lev_oa in namelist record9 for the equivalent pressure tolerance for creating objective analyses.  Note that max_p_extend_t and max_p_extend_w are ignored if use_p_tolerance_one_lev = .TRUE.

Namelist record5
The data in &record5 control the enormous amount of printout that may be produced by the OBSGRID program. These values are all logical flags, where TRUE will generate output and FALSE will turn off output.
print_obs_files ; print_found_obs ; print_header ; print_analysis ;print_qc_vert ; print_qc_dry ; print_error_max ; print_buddy ;print_oa

Namelist record7
The data in &record7 concern the use of the first-guess fields and surface FDDA analysis options. Always use the first guess. 
Namelist Variable	Value	Description
use_first_guess	.TRUE.	Always use first guess (use_first_guess=.TRUE.)
f4d	.TRUE.	Turns on (.TRUE.) or off (.FALSE.) the creation of surface analysis files.
intf4d	10800	Time interval in seconds between surface analysis times
lagtem	.FALSE.	Use the previous time-period's final surface analysis for this time-period's first guess (lagtem=.TRUE.); or 
Use a temporal interpolation between upper-air times as the first guess for this surface analysis (lagtem = .FALSE.)

Namelist record8 
The data in &record8 concern the smoothing of the data after the objective analysis. Note, only the differences fields (observation minus first-guess) of the analyzed are smoothed, not the full fields.
Namelist Variable	Value	Description
smooth_type	1	1 = five point stencil of 1-2-1 smoothing; 2 = smoother-desmoother
smooth_sfc_wind	0	Number of smoothing passes for surface winds
smooth_sfc_temp	0	Number of smoothing passes for surface temperature
smooth_sfc_rh	0	Number of smoothing passes for surface relative humidity
smooth_sfc_slp	0	Number of smoothing passes for sea-level pressure
smooth_upper_wind	0	Number of smoothing passes for upper-air winds
smooth_upper_temp	0	Number of smoothing passes for upper-air temperature
smooth_upper_rh	0	Number of smoothing passes for upper-air relative humidity

Namelist record9 
The data in &record9 concern the objective analysis options. There is no user control to select the various Cressman extensions for the radius of influence (circular, elliptical or banana). If the Cressman option is selected, ellipse or banana extensions will be applied as the wind conditions warrant.
Namelist Variable	Value	Description
oa_type	“Cressman”	“MQD” for multiquadric; “Cressman” for the Cressman-type scheme, "None" for no analysis, this string is case sensitive
oa_3D_type	“Cressman”	Set upper-air scheme to “Cressman”, regardless of the scheme used at the surface
oa_3D_option	0	How to switch between “MQD” and “Cressman” if not enough observations are available to perform “MQD”
mqd_minimum_num_obs	30	Minimum number of observations for MQD
mqd_maximum_num_obs	1000	Maximum number of observations for MQD
radius_influence	5,4,3,2	Radius of influence in grid units for Cressman scheme
radius_influence_sfc_mult	1.0	Multiply above-surface radius of influence by this value to get surface radius of influence
oa_min_switch	.TRUE.	T = switch to Cressman if too few observations for MQD; F = no analysis if too few observations
oa_max_switch	.TRUE.	T = switch to Cressman if too many observations for MQD; F = no analysis if too many observation
scale_cressman_rh_decreases	.FALSE.	T = decrease magnitude of drying in Cressman analysis; F = magnitude of drying in Cressman analysis unmodified
oa_psfc	.FALSE.	T = perform surface pressure objective analysis; F = surface pressure only adjusted by sea level pressure analysis
max_p_tolerance_one_lev_oa	700	Pressure tolerance within which single-level above-surface observations can be used in the objective analysis (Pa)
When oa_type is set to Cressman, then the Cressman scheme will be performed on all data.
When oa_type is set to None, then no objective analysis will be performed on any data.
When oa_type is set to MQD, there are a wide variety of options available that control when the code will revert back to the Cressman scheme. 
•	oa_max_switch ; mqd_maximum_num_obs
The code will revert back to Cressman if the switch is set to true and the maximum number of observations is exceeded.
This is to reduce the time the code runs and not for physical reasons.
Recommended to leave switch set to true and just set the maximum number large.

•	oa_min_switch ; mqd_minimum_num_obs
The code will revert back to Cressman if the switch is set to true and there are too few  observations. How and when the code reverts back to Cressman under these conditions are controlled by the oa_3D_option parameter.
Recommended to leave switch set to true and start with the default minimum settings.

•	oa_3D_type=”Cressman”
All upper-air levels will use the Cressman scheme, regardless of other settings.
 
The surface will use MQD as long as there are enough observations to do so (mqd_maximum_num_obs ; mqd_minimum_num_obs), otherwise it will revert to the Cressman scheme. 
Note that if some time periods have enough observations and others do not, the code will only revert to Cressman for the times without sufficient observations. 

•	oa_3D_option
There are three options (0,1,2). For all these options the surface will use MQD as long as there are enough observations to do so (mqd_maximum_num_obs ; mqd_minimum_num_obs), otherwise it will revert to the Cressman scheme. 
Note that if some time periods have enough observations and others do not, the code will only revert to Cressman for the times without sufficient observations.

The upper-air will react as follows:
0 (default): MQD is performed in the upper-air as long as there are enough observations to do so (mqd_maximum_num_obs ; mqd_minimum_num_obs). As soon as this is no longer the case, the code will STOP, with suggestions as to which parameters to set to run the code correctly. 

1: The code will first check to see if, for a given time, all levels and variables in the upper-air have sufficient observations for the MQD scheme. If not, the code will revert to Cressman for that time period. Note that if some time periods have enough observations and others do not, the code will only revert to Cressman for the times without sufficient observations.

2: The code will check if sufficient observations are available per time, level, and variable for the MQD scheme. If not, the code will revert to the Cressman scheme for that particular time, level and variable. Note this can result in uncontrolled switching between MQD and Cressman.  Therefore this option is not recommended. 
radius_influence
There are three ways to set the radius of influence (RIN) for the Cressman scheme:
•	Manually: Set the RIN and number of scans directly. E.g., 5,4,3,2, will result in 4 scans. The first will use 5 grid points for the RIN and the last, 2 points.
•	Automatically 1: Set RIN to 0 and the code will calculate the RIN based on the domain size and an estimated observation density of 325 km. By default there will be 4 scans.
•	Automatically 2: Set RIN to a negative number and the code will calculate the RIN based on the domain size and an estimated observation density of 325 km. The number of scans is controlled by the value of the set number. E.g, -5 will result in 5 scans. 

radius_influence_sfc_mult 
The RIN calculated as described above is multiplied by this value to determine the RIN for surface observations.  This allows the finer scale structures observed at the surface to be retained.  If this multiplication results in a RIN greater than 100 model grid points, then the RIN on the first scan is scaled to be 100 model grid points and all subsequent scans are scale by that same ratio.  This is to prevent features from being washed out on fine-scale domains.  In order to minimize “spots” on the solution, any scan with a RIN less than 4.5 model grid points is skipped. If this is set to 1.0 then the RIN for surface observations will match the RIN for above-surface observations.

scale_cressman_rh_decreases 
This option is meant to mitigate overdrying that can occur when the need for drying diagnosed via an observation at one point is spread to another point where the first guess is already drier than the first guess at the location of the observation  If this option is set to true then drying applied to a point where the first guess is drier than the first guess at the observation location is scaled by the ratio first guess relative humidity at the point the drying is being applied to divided by the first guess relative humidity at the location of the observation. 
 
Note that this scaling is applied on each Cressman scan.  See Reen et al. 2016 (http://dx.doi.org/10.1175/JAMC-D-14-0301.1) for further details.
 
oa_psfc 
An objective analysis of surface pressure may allow Obsgrid surface analyses of other fields to be more effectively utilized in WRF if the first-guess surface pressure field is sufficiently coarse compared to the WRF domains (e.g., Reen 2015; http://www.arl.army.mil/arlreports/2015/ARL-TR-7447.pdf).  This is because the surface pressure analysis may provide a better estimate of the pressure of the surface analyses and thus WRF is less likely to erroneously reject the surface analyses as being too distant from the actual surface.  If there are an insufficient number of observations or if the first-guess surface pressure is not much coarser than WRF, this capability is less likely to add value.
 
max_p_tolerance_one_lev_oa
If use_p_tolerance_one_lev = .TRUE. in record4, then max_p_tolerance_one_lev_oa is the pressure tolerance (Pa) allowed between single-level above-surface observations and the pressure level they are being used in an objective analysis. If use_p_tolerance_one_lev = .FALSE. in record4, then max_p_tolerance_one_lev_oa is not used by OBSGRID.

Namelist plot_sounding
Only used for the utility plot_sounding.exe
Namelist Variable	Value	Description
file_type	“raw”	File to read to produce the plots. Options are “raw” or “used”
read_metoa	.TRUE.	If set to .TRUE., the model domain information in the metoa_em files will be used to add location information on the plot.


