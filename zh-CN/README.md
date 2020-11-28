# 前言

这本用户手册针对2018年6月发布的ARW（ Advanced Research WRF）4.0版本。因为WRF/ARW一直在进一步的开发过程中，所以本用户手册也会不断增加和更新内容。如有任何意见和建议，请反馈到[WRF & MPAS-A Support Forum]( http://forum.mmm.ucar.edu/ )。

本手册是对[ARW技术说明]( http://www.mmm.ucar.edu/wrf/users/docs/arw_v3.pdf )的补充，该技术说明更详细地描述了相关的方程式、数值模拟、边界条件和嵌套等内容。（V4版本的的技术说明仍在编辑中。）

WRF V4.0版本主要更新点如下：

* WRF模型：
	* 动力学和求解器：
		* 在V3.9版本中引入的混合sigma压力垂直坐标现在作为默认值
		* 默认情况下，温度的预测变量是湿位温度
	* 物理学：
		* 两个自由类别的P3微物理（由Morrison和Milbrandt提供）
		* 汤普森-艾德哈默微物理学（NCAR的汤普森、徐、T.艾德哈默）增加了一个地表粉尘排放系统
		* 在多尺度Kain-Fritsch系统中加入了CESM气溶胶，其中包括Song-Zhang微物理方案和Morrison系统（Timothy Glotfelty、Patrick Hawbecker和EPA的Kiran Alapaty）
		* 在NoahMP中加入作物生长模拟模型中的基因型-无机环境的相互作用（U.Hohenheim的J.Ingwersen和NCAR的M.Barlage）
		* 在NoahMP（NCAR的M.Barlage）中可选择使用土壤成分数据
		* 一个尺度感知SAS系统和RRTMG-K（韩国建立的大气预报系统）
		* 更新了WRF的森林火灾代码（fire code）（NCAR的Domingo Munoz Esparza）
		* 更新了RAP/HRRR和其他物理方面的信息

* WPS：
	* 重组输入静态数据；
	* 0.05度MODIS反照率和积雪反照率（NCAR Barlage）

* WRF-DA：
	* GOES成像仪辐射率（中国南京信息科技大学的C.Yang和NCAR的Z.Liu）
	* GPSRO过相位观测算子GPSRO excess phase observation operator
	* 大范围分析约束与发散约束（南京大学汤学文）
	* 在WRF子目录wrftladj/中集成WRFPlus代码

* WRF化学：
	* 基于平流层臭氧计算的潜在涡度（S.McKeen，NOAA）
	* 模拟气溶胶相互作用和化学Aerol反应系统模型的更新版本（MOSAIC II）（J.PNNL的Fast）
	* 一个更新的异构气体化学选项耦合到Iropropii II气溶胶热力学模型（Q.香港理工学院）
	* 综合反应速率诊断选项（S.Walters等人，NCAR）
	* 辐射驱动器中WRF化学气溶胶辐射反馈的诊断（D.Lowe，S.Archer Nichols，英国剑桥大学）
	* 亨利定律常数表，用于在不同的化学参数化中使用相同的常数，例如干/湿沉积方案（M.Barth等人，NCAR）

有关本文档的最新版本，请访问[ARW用户网站]( http://www2.mmm.ucar.edu/wrf/users/ )。

# 目录

1. [概述](users_guide_chap1.md)

1.1 [简介](users_guide_chap1.md#Introduction)

1.2 [WRF模型系统的构成](users_guide_chap1.md#WRF_Modeling_System)

2. Software Installation

2.1 Introduction

2.2 Required Compilers and Scripting Languages

2.3 Required/Optional Libraries to Download

2.4 Post-Processing Utilities

2.5 UNIX Environment Settings

2.6 Building the WRF Code

2.7 Building the WPS Code

3. WRF预处理系统（WRF Preprocessing System，WPS）

3.1 介绍

	Function of Each WPS Program	3-2
	Installing the WPS	3-5
	Running the WPS	3-8
	Creating Nested Domains with the WPS	3-20
	Selecting Between USGS and MODIS-based 
Land Use Data	3-22
	Selecting Static Data for the Gravity Wave Drag Scheme	3-23
	Using Multiple Meteorological Data Sources	3-23
	Using Non-isobaric Meteorological Datasets .............................3-26
	Alternative Initialization of Lake SSTs…………………………… 3-27
	Parallelism in the WPS	3-28
	Checking WPS Output	3-29
	WPS Utility Programs	3-30
	Writing Meteorological Data to the Intermediate Format	3-34
	Required Meteorological Fields for Running WRF.....................3-36
	Using MPAS Output for WRF Initial and Boundary Conditions..3-37
	Creating and Editing Vtables	3-39
	Writing Static Data to the Geogrid Binary Format	3-41
	Creating an Urban Fraction Field from NLCD Data .................. 3-44
	Description of Namelist Variables	3-46
	Description of GEOGRID.TBL Options	3-52
	Description of index Options	3-55
	Description of METGRID.TBL Options	3-58
	Available Interpolation Options in Geogrid and Metgrid	3-61
	Land Use and Soil Categories in the Static Data	3-64
	WPS Output Fields	3-66


4. WRF Initialization
	Introduction	4-1
	Initialization for Ideal Cases	4-3
	Initialization for Real Data Cases	4-6

5. WRF Model
	Introduction 	5-2
	Installing WRF 	5-2
	Running WRF 	5-8
	Examples of namelists for various applications	5-38
	Check Output 	5-40
	Trouble Shooting	5-41
	Physics and Dynamics Options	5-42
	Summary of PBL Physics Options……………………………….  5-58
	Summary of Microphysics Options………………………………. 5-59
	Summary of Cumulus Parameterization Options………………. 5-62
	Summary of Radiation Physics Options.................................... 5-63
	Description of Namelist Variables	5-66
	WRF Output Fields	5-122
	Special WRF Output Variables.................................................5-130

6. WRF Data Assimilation
	Introduction	6-2
	Installing WRFDA for 3DVAR Run….	6-4
	Installing WRFPLUS and WRFDA for 4DVAR Run	6-9
	Running Observation Preprocessor (OBSPROC) 	6-10
	Running WRFDA	6-14
	Radiance Data Assimilations in WRFDA	6-23
	Radar Data Assimilation in WRFDA...........................................6-34
	Precipitation Data Assimilation in WRFDA 4D-Var…………….. 6-37
	Updating WRF boundary conditions	.6-39
	Background Error and Running GEN_BE...................................6-43
	WRFDA Diagnostics	6-51
	Generating Ensembles with RANDOMCV..................................6-55
	Hybrid Data Assimilation in WRFDA	6-56
	ETKF Data Assimilation............................................................. 6-62
	Additional WRFDA Options	6-67
	Description of Namelist Variables	6-70

7. Objective Analysis (OBSGRID) 
	Introduction	7-1
	Program Flow	7-2
	Source of Observations	7-3
	Objective Analysis techniques in OBSGRID	7-4
	Quality Control for Observations	7-6
	Additional Observations	7-7
	Surface FDDA option	7-7
	Objective Analysis on Model Nests	7-8
	How to run OBSGRID	7-8
	Output Files	7-10
	Plot Utilities	7-13
	Observations Format	7-15
	OBSGRID Namelist	7-19

8. WRF Software
	WRF Build Mechanism	8-1
	Registry	8-5
	I/O Applications Program Interface (I/O API)	8-14
	Timekeeping	8-14
	Software Documentation	8-15
	Performance	8-15

9. Post-Processing Programs
	Introduction	9-1
	NCL		9-2
	RIP		9-20
	ARWpost	9-29
	UPP 		9-36
	VAPOR	9-38

10. Utilities and Tools
	Introduction	10-1
	read_wrf_nc	10-1
	iowrf		10-5
	p_interp	10-6
	TC Bogus Scheme	10-10
	v_interp	10-12
	proc_oml.f	10-14
	Tools		10-15

Appendix A: WRF-Fire 
	Introduction	A-1
	WRF_Fire in idealized cases	A-3
	Fire variables in namelist.input 	A-4
	namelist.fire	A-6
	Running WRF_Fire on real data	A-7
	Fire state variables	A-13
	WRF-Fire software 	A-13