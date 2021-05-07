# 前言

这本用户手册针对2021年5月发布的ARW（ Advanced Research WRF）4.3版本。因为WRF/ARW一直在进一步的开发过程中，所以本用户手册也会不断增加和更新内容。如有任何意见和建议，请反馈到[WRF & MPAS-A Support Forum]( http://forum.mmm.ucar.edu/ )。

本手册是对[ARW技术说明]( https://opensky.ucar.edu/islandora/object/opensky:2898 )的补充，该技术说明更详细地描述了相关的方程式、数值模拟、边界条件和嵌套等内容。

WRF V4.3版本主要更新点如下：

* WRF模型：
	* 动力学和求解器：
		* 添加了隐式显式垂直对流（Implicit Explicit Vertical Advection，IEVA）选项，该选项可以帮助提高对流分辨尺度的模型稳定性。
	* 物理学：
		* 国立台湾大学微物理方案，由Tsai和Chen贡献
		* P3微物理方案 one ice，3-moment，由Morrison和Milbrandt贡献
		* 添加了基于湍流动能（turbulent kinetic energy，TKE）和TKE耗散率（ε）的1.5阶闭合PBL参数化（E–ε，EEPS），由夏威夷大学的Zhang和Wang贡献
		* 一套新的重力波阻力方案，由NOAA/GSL的Mike Toy贡献
		* 增加了用来与YSU PBL配合使用的城市物理学方案，由NCAR的Hendricks贡献
		* 能够使用来自WUDAPT的当地气候区数据，由意大利特伦托大学的Zonato贡献
		* 用于BEB+BEM的绿色屋顶、太阳能电池板和新的建筑阻力系数，由Zonato贡献
		* 用于NoahMP的动态灌溉管理，由NCAR的Valayamkunnath贡献
		* 为某些辐射物理学增加了日食效应，由西班牙巴塞罗那大学的蒙托尔内斯和肯塔基大学的罗维共同贡献
 
* WPS：
	* 添加了重力波阻力选项3的新字段，由NOAA的Mike Toy贡献
	* 添加了NoahMP的灌溉数据，由NCAR的Valayamkunnath贡献

* WRF-DA：
	* 多分辨率增量式4DVAR，由NCAR的Liu，Ban，Bresch，Wu，X.Zhang和J.Liu贡献
	* 地面化学数据同化的WRFDA扩展，由NCAR的Sun，Liu，中国IUM/CMA的Chen贡献

* WRF化学：
	* 更新了CLM土地模型，使其与MEGAv2.1生物成因排放模型保持一致，由PNNL的Gaudet贡献

有关本文档的最新版本，请访问[ARW用户网站]( http://www2.mmm.ucar.edu/wrf/users/ )。

# 目录

1. [概述](users_guide_chap1.md)

	1.1 [简介](users_guide_chap1.md#Introduction)

	1.2 [WRF模型系统的构成](users_guide_chap1.md#WRF_Modeling_System)

2. [软件安装](users_guide_chap2.md)

	2.1. [简介](users_guide_chap2.md#Introduction)

	2.2. [需要的编译器和脚本语言](users_guide_chap2.md#Compilers_Scripting)

	2.3. [需要/可选的库文件](users_guide_chap2.md#Libraries)

	2.4. [后处理工具](users_guide_chap2.md#Post_Processing)

	2.5. [UNIX环境设置](users_guide_chap2.md#UNIX_Environment)

	2.6. [编译WRF核心](users_guide_chap2.md#Building_WRF)

	2.7. [编译WPS核心](users_guide_chap2.md#Building_WPS)

3. [WRF预处理系统（WRF Preprocessing System，WPS）](users_guide_chap3.md)

	3.1 [简介](users_guide_chap3.md#Introduction)

	3.2 [每个WPS程序的功能](users_guide_chap3.md#Function_of_Each)

	3.3 [安装WPS](users_guide_chap3.md#How_to_Install)

	3.4 [运行WPS](users_guide_chap3.md#How_to_Run)

	3.5 [使用WPS创建嵌套域](users_guide_chap3.md#Using_WRFSI_for_NESTED)

	3.6 [在基于USGS和MODIS的土地利用分类之间进行选择](users_guide_chap3.md#Selecting_Between_USGS_MODIS)

	3.7 [为重力波拖曳方案选择静态数据](users_guide_chap3.md#Selecting_Static_Data)

	3.8 [使用多种气象数据源](users_guide_chap3.md#Using_Multiple_Meteorological)

	3.9 [使用非等压气象数据集](users_guide_chap3.md#Using_Non-isobaric_Meteorological)

	3.10 [湖SSTs的替代初始化](users_guide_chap3.md#Alternative_Initialization_of_Lake)

	3.11 [WPS的并行计算](users_guide_chap3.md#Parallelism_in_WPS)

	3.12 [检查WPS输出](users_guide_chap3.md#Checking_WPS_Output)

	3.13 [WPS实用程序](users_guide_chap3.md#WPS_Utility_Programs)

	3.14 [将气象数据写入中间格式](users_guide_chap3.md#Writing_Meteorological_Data)

	3.15 [运行WRF所需的气象场](users_guide_chap3.md#Required_Meteorological_Fields)

	3.16 [将MPAS输出用于WRF初始条件和横向边界条件](users_guide_chap3.md#Using_MPAS_Output)

	3.17 [创建和编辑Vtables](users_guide_chap3.md#Creating_and_Editing_Vtables)

	3.18 [将静态数据写入Geogrid二进制格式](users_guide_chap3.md#Writing_Static_Data)

	3.19 [从NLCD数据创建城市分数字段](users_guide_chap3.md#Creating_Urban_Fraction)

	3.20 [名称列表变量说明](users_guide_chap3.md#Namelist_Variables)

	3.21 [Geogrid.TBL选项说明](users_guide_chap3.md#Geogrid_TBL_Options)

	3.22 [索引选项说明](users_guide_chap3.md#index_Options)

	3.23 [METGRID.TBL选项说明](users_guide_chap3.md#METGRID_TBL_Options)

	3.24 [Geogrid和Metgrid中可用的插值选项](users_guide_chap3.md#Available_Interpolation_Options)

	3.25 [静态数据中的土地利用和土壤类别](users_guide_chap3.md#Land_Use_and_Soil_Categories)

	3.26 [WPS输出字段](users_guide_chap3.md#WPS_Output_Fields)


4. [WRF初始化](users_guide_chap4.md)

	4.1 [简介](users_guide_chap4.md#Introduction)

	4.2 [Ideal数据案例的初始化](users_guide_chap4.md#Initialization_for_Ideal)

	4.3 [Real数据案例的初始化](users_guide_chap4.md#Initialization_for_Real)

5. [WRF模型](users_guide_chap5.md)

	5.1 [简介](users_guide_chap5.md#Introduction)

	5.2 [安装WRF](users_guide_chap5.md#Installing_WRF)

	5.3 [运行WRF](users_guide_chap5.md#Running_WRF)	
	
	5.4 [各种应用的namelist示例](users_guide_chap5.md#Examples_namelists)

	5.5 [检查输出文件](users_guide_chap5.md#Check_Output)

	5.6 [故障排除](users_guide_chap5.md#Trouble_Shooting)

	5.7 [物理与动力学选项](users_guide_chap5.md#Physics_Dynamics)

	5.8 [PBL物理选项摘要](users_guide_chap5.md#PBL_Physics)

	5.9 [微物理学选项摘要](users_guide_chap5.md#Microphysics)

	5.10 [积云参数化选项摘要](users_guide_chap5.md#Cumulus_Parameterization)

	5.11 [辐射物理选项摘要](users_guide_chap5.md#Radiation)

	5.12 [namelist变量描述](users_guide_chap5.md#Namelist_Variables)

	5.13 [WRF输出字段](users_guide_chap5.md#Output_Fields)

	5.14 [特殊的WRF输出变量](users_guide_chap5.md#Special_Output)

6. [WRF数据同化](users_guide_chap6.md)

	Introduction	6-2

	Installing WRFDA for 3DVAR Run  6-4

	Installing WRFPLUS and WRFDA for 4DVAR Run	6-9

	Running Observation Preprocessor (OBSPROC) 	6-10

	Running WRFDA	6-14

	Radiance Data Assimilations in WRFDA	6-23

	Radar Data Assimilation in WRFDA  6-34

	Precipitation Data Assimilation in WRFDA 4D-Var  6-37

	Updating WRF boundary conditions	6-39

	Background Error and Running GEN_BE  6-43

	WRFDA Diagnostics	6-51

	Generating Ensembles with RANDOMCV  6-55

	Hybrid Data Assimilation in WRFDA	6-56

	ETKF Data Assimilation  6-62

	Additional WRFDA Options	6-67

	Description of Namelist Variables	6-70

7. [客观分析（OBSGRID）](users_guide_chap7.md)

	7.1. [简介](users_guide_chap7.md#Introduction)

	7.2. [程序流程](users_guide_chap7.md#Program_Flow)

	7.3. [观测数据来源](users_guide_chap7.md#Source_of_Obs)

	7.4. [OBSGRID中使用的客观分析技术](users_guide_chap7.md#OA_tech)

	7.5. [观测质量控制](users_guide_chap7.md#QC)

	7.6. [附加观测](users_guide_chap7.md#Additional_Obs)

	7.7. [地面FDDA选项](users_guide_chap7.md#Surface_FDDA)

	7.8. [模型嵌套中的客观分析](users_guide_chap7.md#OA_Nests)

	7.9. [如何运行OBSGRID](users_guide_chap7.md#How_to_run)

	7.10. [输出文件](users_guide_chap7.md#Output)

	7.11. [图形工具](users_guide_chap7.md#Plot_Utilities)

	7.12. [观测格式](users_guide_chap7.md#Obs_Format)

	7.13. [OBSGRID Namelist](users_guide_chap7.md#Namelist)

8. [WRF软件](users_guide_chap8.md)

	WRF Build Mechanism	8-1

	Registry	8-5

	I/O Applications Program Interface (I/O API)	8-14

	Timekeeping	8-14

	Software Documentation	8-15

	Performance	8-15

9. [后处理程序](users_guide_chap9.md)

	Introduction	9-1

	NCL		9-2

	RIP		9-20

	ARWpost	9-29

	UPP 		9-36

	VAPOR	9-38

10. [实用工具](users_guide_chap10.md)

	Introduction	10-1

	read_wrf_nc	10-1

	iowrf		10-5

	p_interp	10-6

	TC Bogus Scheme	10-10

	v_interp	10-12

	proc_oml.f	10-14

	Tools		10-15

11. [附录A WRF-Fire](Appendix_A)

	Introduction	A-1

	WRF_Fire in idealized cases	A-3

	Fire variables in namelist.input 	A-4

	namelist.fire	A-6

	Running WRF_Fire on real data	A-7

	Fire state variables	A-13

	WRF-Fire software 	A-13