# 前言

这本用户手册针对2018年6月发布的ARW（ Advanced Research WRF）4.0版本。因为WRF/ARW一直在进一步的开发过程中，所以本用户手册也会不断增加和更新内容。如有任何意见和建议，请反馈到[WRF & MPAS-A Support Forum]( http://forum.mmm.ucar.edu/ )。

本手册是对[ARW技术说明]( http://www.mmm.ucar.edu/wrf/users/docs/arw_v3.pdf )的补充，该技术说明更详细地描述了相关的方程式、数值模拟、边界条件和嵌套等内容。（V4版本的的技术说明仍在编辑中。）

WRF V4.0版本主要更新点如下：

	WRF模型：
	动力学和求解器：
		在V3.9版本中引入的混合sigma压力垂直坐标现在作为默认值
		默认情况下，温度的预测变量是湿位温度
	物理学：
		两个自由类别的P3微物理（由Morrison和Milbrandt提供）
		汤普森-艾德哈默微物理学（NCAR的汤普森、徐、T.艾德哈默）增加了一个地表粉尘排放系统
		在多尺度Kain-Fritsch系统中加入了CESM气溶胶，其中包括Song-Zhang微物理方案和Morrison系统（Timothy Glotfelty、Patrick Hawbecker和EPA的Kiran Alapaty）
		在NoahMP中加入作物生长模拟模型中的基因型-无机环境的相互作用（U.Hohenheim的J.Ingwersen和NCAR的M.Barlage）
		在NoahMP（NCAR的M.Barlage）中可选择使用土壤成分数据
		一个尺度感知SAS系统和RRTMG-K（韩国建立的大气预报系统）
		更新了WRF的森林火灾代码（fire code）（NCAR的Domingo Munoz Esparza）
		更新了RAP/HRRR和其他物理方面的信息

 	WPS：
		重组输入静态数据；
		0.05度MODIS反照率和积雪反照率（NCAR Barlage）

 	WRF-DA：
		GOES成像仪辐射率（中国南京信息科技大学的C.Yang和NCAR的Z.Liu）
		GPSRO过相位观测算子GPSRO excess phase observation operator
		大范围分析约束与发散约束（南京大学汤学文）
		在WRF子目录wrftladj/中集成WRFPlus代码

 	WRF化学：
		基于平流层臭氧计算的潜在涡度（S.McKeen，NOAA）
		模拟气溶胶相互作用和化学Aerol反应系统模型的更新版本（MOSAIC II）（J.PNNL的Fast）
		一个更新的异构气体化学选项耦合到Iropropii II气溶胶热力学模型（Q.香港理工学院）
		综合反应速率诊断选项（S.Walters等人，NCAR）
		辐射驱动器中WRF化学气溶胶辐射反馈的诊断（D.Lowe，S.Archer Nichols，英国剑桥大学）
		亨利定律常数表，用于在不同的化学参数化中使用相同的常数，例如干/湿沉积方案（M.Barth等人，NCAR）

有关本文档的最新版本，请访问[ARW用户网站]( http://www2.mmm.ucar.edu/wrf/users/ )。