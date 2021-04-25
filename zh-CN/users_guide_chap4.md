# 第四章 WRF初始化

## 目录

1. [简介](#Introduction)

2. [Ideal数据案例的初始化](#Initialization_for_Ideal)

3. [Real数据案例的初始化](#Initialization_for_Real)

<a id=Introduction></a>

## 简介

[WRF模型](http://wrf-model.org/ )有两类模拟，一个是ideal初始化，一个是利用real数据。理想化的模拟通常根据现有的一维或二维测深为WRF模型生成初始条件文件，并假设简化了分析地形。Real数据案例通常需要从WPS软件包中进行预处理，该软件包可为每个大气场和静态场提供适合模型所选网格分辨率的保真度。通过选择一个初始化选项而不是另一个初始化选项（ideal理想化还是real真实化），不会更改WRF模型可执行文件本身，但是WRF模型预处理器（real.exe和Ideal.exe程序）是根据用户的选择专门构建的。在运行WRF模型之前，需要运行real.exe或Ideal.exe。

理想化模型和真实模型区别如下：

* 理想化模型——ideal.exe
	* 3d
		* em_b_wave：斜压波，100km
		* em_convrad：对流辐射，1 km
		* em_fire：地表火，50m
		* em_heldsuarez：带极性滤波的全局情况，625km
		* em_les：大涡模拟，100m
		* em_quarter_ss：超晶胞，2km
		* em_tropical_cyclone：飓风，15km
	* 2d
		* em_grav2d_x：重力流，密度流，100m
		* em_hill2d_x：flow over  a hill(未知) ,2km
		* em_seabreeze2d_x：水陆 ，2km，全物理结构 
		* em_squall2d_x：暴风雨线，阵风线，250m
		* em_squall2d_y：em_squall2d_x的转置
	* 1d
		* em_scm_xy：单柱模型，4km，全物理模型

* 真实模型——real.exe
	* em_real：从4到30km的例子，全物理模型

发出`./compile`语句时，将选择预测类型。选择不同的案例进行研究时，必须重新编译代码以为模型选择正确的初始化。例如，在配置了体系结构的设置（使用`./configure`命令）之后，如果用户发出命令`./compile em_real`，那么初始化程序将使用`module_initialize_real.F`（`./WRF/dyn_em/module_initialize_*.F`文件之一）作为目标模块。

对于ideal的初始化，可以使用文件的组合来构建可执行文件。对于em_fire、e m_heldsuarez、em_scm_xy和em_tropical_cyclone情况，存在单独的初始化文件（例如，module_initialize_fire.F用于em_fire案例）。对于其他理想化情况，将使用文件`./WRF/dyn_em/module_initialize_ideal.F`。要注意WRF预测模型对这两种初始化程序都是一样的。对于每个初始化模块，发生相同类型的活动：

* 计算位势和柱压的基态/参考剖面

* 计算位势和柱压的基态扰动

* 初始化气象变量：u，v，位温，水汽混合比

* 定义垂直坐标

* 将数据插值到模型的垂直坐标

* 初始化地图投影和物理表面的静态字段；对于许多理想情况，这些都是简化的初始化，例如将地图因子设置为1，将地形高程设置为0

* 在退出处理这些预处理活动的例程之前，如果用户要求热场使用潮湿的势能温度，则会计算出诊断变量

real.exe和ideal.exe程序共享很大一部分源代码，以处理以下任务：

* 从namelist中读取数据

* 给划定的区域分配空间，并在运行时指定模型变量

* 生成初始条件文件

real数据案例会进行一些其他处理： 

* 从WRF预处理系统（WPS）读取气象和静态输入数据 

* 准备要在模型中使用的土壤字段（通常垂直插值到指定陆地表面方案所需的层） 

* 检查以确认土壤类别、土地利用、陆地遮罩、土壤温度、海表温度是否彼此一致 

* 处理多个输入时间段以生成横向边界条件，除非处理全球预测，否则这是必需的 

* 3d边界数据（u、v、位温、蒸汽混合比、总地势）与总柱压耦合 

real.exe程序可以作为串行或分布式内存作业运行。由于ideal情况仅要求初始化运行单个时间段（不需要横向边界文件），可以快速处理，因此所有ideal.exe程序都应在单个处理器上运行。用于2D情况的Makefile将不允许用户使用分布式内存并行性来构建代码。对于大型二维情况，如果用户需要OpenMP，则必须在名称列表文件`namelist.input`的`domains`部分中设置变量`numtiles_x`和`numtiles_y`（`numtiles_y`必须设置为1，`numtiles_x`设置为OpenMP线程数）。

<a id=Initialization_for_Ideal></a>

## Ideal数据案例的初始化

ideal.exe可以使用户运行受控方案。一般来说，该运行程序只需要`namelist.input`和`input_sounding`作为输入文件。也有一些例外情况，例如对于斜波情况，需要使用二维二进制测深文件。程序输出`wrfinput_d01`文件，该文件被wrf.exe执行程序读取，即wrf.exe的输入文件。由于Ideal理想化模型中不需要额外的数据，因此即使对于研究real数据情况的研究人员来说，理想化模拟也是确保不同结构和编译器下WRF模型运行正常的简单验证方法。

在理想化模拟中，可以使用任意的边界条件（除了`specified`之外），而且一般来说，并不是为了运行复杂的物理机制。大多数都没有辐射、地表通量或摩擦效应（除了海风案例、LES和全球Held-Suarez）。理想化 情况最常用于动力学研究、再生收敛解或其他已知解以及理想化云模型。同样也有例外，热带气旋案例只缺乏辐射方案，而海风案例则有完整的参数化选项。

理想化情况下有带或不带地形、带或不带初始热扰动的一维、二维、三维示例 ，namelist文件控制模拟区域的大小、垂直层的数量、模型顶部高度、网格大小、时间步长、扩散和阻尼特性、边界条件和物理选项。每个案例目录中的默认namelist中已经存在大量设置。

`input_sounding`文件（在合适的案例目录下）可以是任何层的集合，这些层至少可以达到namelist中的模型顶部高度（ztop）。第一行包括表面压力（hPa）、潜在温度（K）和水分混合比（g/kg）。每一行有五个输入值：高度height（海平面以上高度）、干电位温度（K）、蒸汽混合比（g/kg）、x方向风分量（m/s）、y方向风分量（m/s）。ideal.exe程序从`input_sounding`文件进行数据插值，如果数据不足还可以进行相关推断得到。

理想化模型的基态探测是初始探测，去掉了水分，因此不用被单独定义。要注意斜压波案例中：不使用一维的探测输入，因为从`input_jet`文件读入的是初始的三维数组。也就是说对于斜压波案例而言，namelist.input文件不能改变水平或者垂直维度，因为这些已经在`input_jet`文件中定好了。

除了namelist控制选项或探测外，必须通过编辑Fortran代码来进行修改。这些修改包括改变地形、垂直层的分布、初始热汽泡的性质、或者准备一个案例来使用更多的物理量例如陆地表面模型。这些需要编辑的Fortran代码位于`./WRF/dyn_em/module_initialize_[case].F`中，`[case]`是指在程序编译时选择的案例，如`module_initialize_fire.F`或者`module_initialize_ideal.F`。要修改的子例程是`init_domain_rk`。要改变垂直层，需要定义一维数组`znw`，包括全部层，从在k=1的1开始，以在k=kde的0结尾。要改变地形，需要定义二维数组`ht`，如果使用这些边界条件，需要确保其周期性。要改变热扰动气泡，搜索`bubble`字段来定位代码中需要修改的位置。

每个理想化案例都为使用者提供了一组优秀的默认示例。在超单元案例中给出了热气泡的定义方法。在hill2d案例中，设置初始的三维数组时考虑了地形因素，以便于后续的任何地形条件都可以参考该案例。在飑线案例中的一个对称性示例可以检测你的索引修改是否正确。全物理选项在seabreeze2d_x案例中有相关解释。
 
### 可用的理想测试案例

可用的理想测试案例如下：

1. 2-D squall2d_x（test/em_squall2d_x）

	* 使用凯斯勒微物理和300 m^2/s固定粘度的二维飑线（x，z）
	
	* y中使用的周期性条件，以便3D模型生成2D模拟
	
	* v速度应为零，结果中y不应有变化

2. 2-D squall2d_y (test/em_squall2d_y)

	* 和squall2d_x一样，只不过x要旋转为y
	
	* u速度应为零，结果中x不应有变化

3. 3-D quarter-circle shear supercell simulation (test/em_quarter_ss)四分之一圆剪切超晶胞模拟
	
	* 产生左右移动的超晶胞
	
	* 有关详细信息，请参阅测试目录中的README.quarter文件

4. 2-D flow over a bell-shaped hill (x,z) (test/em_hill2d_x)二维钟形山流 
	
	* 半宽10km，网格长2km，高100m，流速10m/s，N=0.01/s，高30km，80层，辐射边界开阔，吸收上边界
	
	* 在线性静水状态下，垂直倾斜波的垂直波长约为6km

5. 3-D baroclinic waves (test/em_b_wave)三维斜压波
	
	* f面上的气压不稳定急流u（y，z）
	
	* 南北对称，东西边界周期性
	
	* 100km网格尺寸，16km顶部，带4km阻尼层
	
	* 41x81点（x，y），64层

6. 2-D gravity current (test/em_grav2d_x)
	
	* 测试用例在Straka et al，INT J NUMER METH FL 17（1）：1中描述-1993年7月22日至15日
	
	* 查看测试目录中的README.grav2d_x文件。

7. 2-D sea breeze (test/em_seabreeze_x)
	
	* 2公里电网规模，20公里顶部，陆地/水域
	
	* 可与全物理、辐射、表面、边界层和陆地选项一起运行

8. 3-D large eddy simulation (test/em_les)
	
	* 100米网格大小，顶部2公里
	
	* 带焊剂的表层物理
	
	* 双周期

9. 3-D Held-Suarez (test/em_heldsuarez)
	
	* 全球域，x方向625公里，y方向556公里，顶部120公里
	
	* 辐射，极滤光片45以上
	
	* x方向周期，y方向极性边界条件

10. 1-D single column model (test/em_scm_xy)
	
	* 4公里电网规模，12公里顶部
	
	* 全物理
	
	* 双周期

11. 3-D surface fire (test/em_fire)
	
	* [地球科学模型开发讨论（GMDD）497-5452011](http://www.geosci-model-dev-discuss.net/4/497/2011/gmdd-4-497-2011.html)
	
	* 50米，顶部4.5公里
	
	* 10:1次网格比率，无物理
	
	* 开放边界

12. 3-D tropical cyclone (test/em_tropical_cyclone)
	
	* Jordan，J METEOR 15，91-971958中描述的测试用例
	
	* 15公里，顶部25公里
	
	* f面（f=0.5e-5，约20N），SST=28C
	
	* 完全物理，简单的辐射冷却，没有积云
	
	* 双周期

13. 3-D convective-radiative equilibrium (test/em_convrad)
	
	* 1km网格尺寸，30km模型顶部
	
	* 热带条件，小f，弱风，恒定海温
	
	* 全物理
	
	* 双周期

<a id=Initialization_for_Real></a>

## Real数据案例的初始化

Real-data WRF案例使用WRF预处理系统（WPS）提供的“real.exe”程序的输入数据，该程序最初是从先前运行的外部分析或预测模型（例如GFS）生成的。

假设要用WRF按照以下要求进行单层嵌套模拟：

* UTC时间2000年1月24日12：00到25日12：00
	
* 原始的Grib数据间隔时间为6小时
	
WPS将会生成下面所示的粗网格文件（从设置开始时间到结束时间，以6h为间隔）：

* met_em.d01.2000-01-24_12：00：00.nc 
	
* met_em.d01.2000-01-24_18：00：00.nc 
		
* met_em.d01.2000-01-25_00：00：00.nc 
	
* met_em.d01.2000-01-25_06：00：00.nc 
	
* met_em.d01.2000-01-25_12：00：00.nc 

`met`指是WPS中`metgrid.exe`程序的输出数据，也是`real.exe`程序的输入数据。`d01`指的是如果有嵌套区域，该数据指的是哪一层嵌套。后面的数列是日期和时间，每个WPS的输出文件只有一个单独时刻的数据。文件的扩展名`.nc`表示WPS输出格式为nc文件，这个必须在real.exe层序中设置文件格式为 netCDF。对于局部区域的预测，`real.exe`程序必须处理多个时间段，以便模型可以使用横向边界文件。而全球的WRF预测只需要一个初始条件。

WPS包为WRF的`real.exe`程序提供数据。

* 数据附在WRF IO API中。除非需要开发其他工具，否则使用WPS包的netCDF选项用于real.exe输入文件。

* 数据已经水平插值到每个变量的正确的格点，风场被正确旋转到WRF模型的地图投影。

* WPS对三维气象数据的要求：压力、u、v、温度、相对湿度、位势高度

* 可选的三维水文气象资料可以在运行时提供到real程序中，但是这些场在粗网格横向边界文件中不会被使用。metgird输出文件中的场有：QR、QC、QS、QI、QG、QH、QNI（雨、云、雪、冰、霰、冰雹的混合比例和数量浓度），这些都是可用的场。

* WPS输出的的三维土壤数据：土壤温度、土壤湿度、土壤水分（可选，取决于WRF模型的物理选择）

* WPS输出的二维气象数据：海平面压力、地表压力、地表风速u和v、地表温度、地表相对湿度、第一猜想地形高程

* WPS输出的可选二维气象数据：海平面温度、实际积雪深度、水当量积雪深度

* 针对物理地表的二维静态数据：地形高程、土地使用类型、土壤质地类别、时间插值的月数据、水陆标识 、输入模型地形的高程

* 二维静态投影数据：地图因子、科里奥利力、投影旋转、计算纬度

* 常量：区域大小、网格间距、日期

* WPS数据可以是等压的，也可以是更广义的垂直坐标，其中每一列的压力都是无变化的
	
* WPS的输出文件可以包括一个垂直坐标系上的气象数据，以及一个完全不同的垂直坐标系上的其他三维数据（例如排放量）

* 所有的三维气象数据（风、温度、高度、湿度、压力）必须有相同的层数，变量的层数也必须完全相同。例如，不允许温度比高度的层数更多。此外，温度场的垂直第8层是700hPa，而水平风分量的垂直第8层是750hPa也是不允许的。
	
### Real数据测试案例：2000年1月24日12时至25日12时 

* 测试数据可以从[WRF download page](http://www.mmm.ucar.edu/wrf/users/download/get_source2.html )下载，在“WRF Model Test Data”列表下，选择一月的数据。这个是74×61、30km范围的区域，区域中心在美国东部。

* 确保代码成功编译（下载中提供了精细网格嵌套的初始数据，因此可以使用基本嵌套选项构建代码），`./WRF/main/real.exe`和`./WRF/main/wrf.exe`必须都存在。

* 在`./WRF/test/em_real`目录下，将一月份案例的namelist复制到默认名称。
	
	* `cp namelist.input.jan00 namelist.input`

* 链接WPS文件（下载的`met_em*`文件）到`./WRF/test/em_real`目录下。
	
* 对单处理器而言，执行real代码，输入命令real.exe（该命令运行这个五段时间的小案例至少要一分钟）。

* 在运行了real.exe程序之后，会生成`wrfinput_d01`和`wrfbdy_d01`文件，这些文件可以直接被WRF模型使用。
	
* 接下来输入命令wrf.exe运行wrf程序，该命令这应该只需要几分钟（在namelist文件中只设置进行12小时的预测）。

* 运行结束后会输出文件`wrfout_d01:2000-01-24_12:00:00`，其中包含以3小时为间隔，共12小时的数据。

### 最新版本的注意事项 

默认行为是包括“湿势温度”选项和“混合垂直坐标”。这两个选项使向后兼容变得困难。

* 湿势温度：use_theta_m = 0 (off), 1 (on)

WRF v3.8中最早引入了湿势温度。在v3.8到v 3.9.1.1中，所有有关moist theta的进程都由模型处理。这导致在主求解器例程中反复来回切换，因此变量grid%t_2的定义取决于例程中的位置。现在已正确移植了代码，以便将moist theta选项合并到real/ideal预处理器中，并且grid%t_2变量的含义始终相同。所有real情况和ideal情况都支持湿势温度选项。因为代码假定输入变量与moist theta的namelist设置一致，所以通常不使用较早版本的wrfinput_d0x和wrfbdy_d01文件。

如果用户具有较旧的输入数据（v4.0之前的版本），并且关闭了moist theta选项，则可以使用wrfinput_d0x和wrfbdy_d01数据。

如果用户打开湿势温度选项，则只能使用新版本的wrfinput_d0x和wrfbdy_d01数据。
 
* 混合垂直坐标（HVC）：hybrid_opt = 0 (off), 2 (on)
	
WRF代码支持混合垂直坐标，但仅适用于所有real数据和ideal情况。与moist theta选项一样，混合垂直坐标也适用同样的禁令。

如果用户具有不使用HVC的较旧的输入数据（v4.0之前的版本），并且用户关闭了WRF模型中的混合垂直坐标选项，则可以使用wrfinput_d0x和wrfbdy_d01数据。

如果用户打开了HVC选项，则只能使用新版本的wrfinput_d0x和wrfbdy_d01数据。

提供了一个namelist选项（force_use_old_data=.TRUE.），以明确允许将旧版本的wrfinput_d0x和wrfbdy_d01文件引入WRF模型。 

### 设置模型垂直层

用户可以使用namelist选项`eta_levels`明确定义完整的eta层。给出了28层和35层的两个分布。层数必须与分配的eta表面数（`e_vert`）一致。用户可以选择仅请求层数（使用`e_vert`），然后real程序将计算得出取值。有两种方法可以选择：auto_levels_opt = 1 (old) or 2 (new)。旧的计算方法假设先有几个已知层，然后生成等高间隔的层，直到模型的顶部。新的方法使用地表和上部拉伸系数（`dz_stretch_s`和`dz_stretch_u`），以从厚度`dzbot`开始，根据log P拉伸层，向上直到最大厚度点（`max_dz`）。当厚度达到`max_dz/2`时，拉伸从`dzstretch_s`过渡到`dzstretch_u`。 
 
当dzbot=50m和max_dz=1000m时，作为dzstretch和p_top函数的最小层数

dzstretch\ptop|50|30|20|10|1
--------------|--|--|--|--|--
1.1           |44|47|50|54|67
1.2           |32|35|37|41|54
1.3①          |28|31|33|37|50

①注：1.3在低于5km时达到1km厚度（13层）–可能不推荐

1.2在7km左右达到1km厚度（19层）

1.1在约13km处达到1km厚度（36层）

 
dzstretch = 1.1，在最低的1km中有12层，在10km以下有34层

dzstretch = 1.2，在最低的1km中有9层，在10km以下有22层

dzstretch = 1.3，在最低的1km中有8层，在10km以下有18层

 
使用dzstretch_s和dzstretch_u时的最小层数

dzstretch\ptop|50|30|20|10|1
--------------|--|--|--|--|--
1.2-1.02      |53|58|62|67|81
1.2-1.04      |46|49|51|55|68
1.2-1.06      |41|44|47|50|63
1.3-1.1       |33|36|39|43|56

为了避免在对流层上部具有最大厚度，在达到常数`d(logp)`之前，拉伸层需要延伸到对流层顶上方。这可以通过使用足够低的`dzstretch_u`值（但大于1.02）来到达对流层顶，同时拉伸速度也足够快，以弥补误差率。
 
还可以使用其他两个namelist来增加灵活性：`dzbot`，这是全部层之间的第一个模型层的厚度（默认值为50 m）；以及`max_dz`，这是允许的最大层的厚度（默认值1000m）。