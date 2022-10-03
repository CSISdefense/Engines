# engines

This folder includes coding work for the Future of Military Engines project. 

There are three main datasets. 

1. **inventory**: USAF aircraft inventory / engines / specs (.../inventory) 

2. **contracts**: Federal Procurement Data System (FPDS) contract numbers for military engines (.../contracts) 

3. **budget**: Future Years Defense Program RDTE funding for military engines (.../budget) 

# 1. inventory 

The purpose of the inventory dataset is to map out the history of USAF engine trends from 1950-present. This includes the number of aircraft, the number of engines, the age of the fleet, and performance specs of the entire fleet. 

**Aircraft inventory**

We began with a 2010 Air Force Association report, “Arsenal of Airpower: USAF Aircraft Inventory 1950-2009." This report provides the number of each platform that make up the USAF Total Aircraft Inventory. We then used the USAF Almanacs from 2010 to 2017 to update the inventory numbers. With this information, we had four variables: aircraft, type, year, and amount. 

**Engine inventory**

We then added a new variable, engine, which identifies the engine for every platform. For instance, the F-35 has the F135 and the F-22 has the F119. Furthermore, we determined the number of engines for each platform and created the variable: engine_amount. For instance, the F-35 only has one engine and the F-22 has two. 

**Aircraft performance specs**

We identified the most relevant and consistently available aircraft performance specs for FighterAttack. These variables included: takeoff weight, speed, range, ceiling, climb rate, and thrust to weight ratio of the aircraft. 

**Engine performance specs** 

We identified the most relevant and consistently available engine performance specs for FighterAttack that had turbojet or turbofan engines. These variables included: maximum thrust, overall pressure ratio, engine weight, and thrust to weight ratio of the engine.

**Weaknesses**

This dataset has two main weaknesses. 1) While it is more comprehensive than any other publicly available dataset on aircraft and engines, it lacks data for some major categories. For example, we did not assign performance specs for other categories beyond FighterAttack and we did not assign engine inventory data to Helicopter or Trainer aircraft. This is due mainly to the limited scope of this project and to the limited sources that have this type of information. 2) For performance specs, we relied heavily on Wikipedia pages. The primary sources listed on these pages were generally reputable (i.e. Jane’s all the World’s Aircraft), especially for heavily produced aircraft. And when the sources were not listed or the numbers were unclear, we found secondary sources or made assumptions based on our analysis of other platforms. Despite these shortcomings, this dataset is a valuable resource for this project because we have a high degree of confidence in the numbers for heavily produced aircraft and because we are focused on overall trend analysis.  

**Inventory variables** 

`aircraft`: the name of each platform 

`type`: the type of aircraft. Includes: Bomber, FighterAttack, Helicopter, Recon, Tanker, Trainer, and Transport

`year`: the fiscal year  

`amount`: the number for each platform in the USAF Total Active Inventory 

`engine`: the name of each engine

`engine_type`: the type of engine. Includes: Radial, Turbofan, Turbojet, Turboprop, and Turboshaft 

`engine_number`: the number of engines on the specific aircraft 

`engine_company`: the main manufacturer for each engine 

`takeoff_weight`: max listed takeoff weight in pounds 

`speed`: max listed speed in mph

`range`: max listed range in mi 

`ceiling`: max listed service ceiling in ft 

`climb_rate`: listed rate of climb in ft/min

`thrust_weight_aircraft`: listed thrust/weight ratio of the aircraft

`thrust`: max listed thrust of the engine in lbs  

`pressure_ratio`: listed overall pressure ratio 

`engine_weight`: listed engine weight in lbs 

`thrust_weight_engine`: listed thurst/weight ratio of the engine 

`intro_year`: the first year that the aircraft appeared in the USAF Total Active Inventory 

`peak_amount`: the max amount for each aircraft between 1950 - present

`generation`: the fighter generation for FighterAttack aircraft 

# 2. contracts

The purpose of the contracts dataset is to identify important trends in contract obligations that are directly relevant to military aircraft engines. 

**Federal Procurement Data System methodology**

For nearly a decade, the Defense-Industrial Initiatives Group (DIIG) has issued a series of analytical reports on federal contract spending for national security across the government. These reports are built on FPDS data, presently downloaded in bulk from USAspending.gov. DIIG now maintains its own database of federal spending, including years 1990–2017, that is a combination of data download from FPDS and legacy DD350 data. For this report, however, the study team primarily relied on FY2000–2017. Data before FY2000 require mixing sources and incur limitations.

**Inherent restrictions of FPDS**

Since the analysis presented in this report relies almost exclusively on FPDS data, it incurs four notable restrictions. First, contracts awarded as part of overseas contingency operations are not separately classified in FPDS. As a result, we do not distinguish between contracts funded by base budgets and those funded by supplemental appropriations. Second, FPDS includes only prime contracts, and the separate subcontract database (Federal Subaward Reporting System, FSRS) has historically been radically incomplete; only in the last few years have the subcontract data started to approach required levels of quality and comprehensiveness. Therefore, only prime contract data are included in this report. Third, reporting regulations require that only unclassified contracts be included in FPDS. We interpret this to mean that few, if any, classified contracts are in the database. For DoD, this omits a substantial amount of total contract spending, perhaps as much as 10 percent. Such omissions are probably most noticeable in R&D contracts. Finally, classifications of contracts differ between FPDS and individual vendors. For example, some contracts that a vendor may consider as services are labeled as products in FPDS and vice versa. This may cause some discrepancies between vendors’ reports and those of the federal government.

**Constant dollars and fiscal years**

All dollar amounts in this data analysis section are reported as constant FY 2016 dollars unless specifically noted otherwise. Dollar amounts for all years are deflated by the implicit GDP deflator calculated by the U.S. Bureau of Economic Analysis, with FY2016 as the base year, allowing the CSIS team to more accurately compare and analyze changes in spending across time. Similarly, all compound annual growth values and percentage growth comparisons are based on constant dollars and thus adjusted for inflation. Due to the native format of FPDS and the ease of comparison with government databases, all references to years conform to the federal fiscal year. FY2017, the most recent complete year in the database, spans from October 1, 2016, to September 30, 2017.

**Data quality**

Any analysis based on FPDS information is naturally limited by the quality of the underlying data. Several Government Accountability Office (GAO) studies have highlighted the problems of FPDS (for example, William T. Woods’ 2003 report “Reliability of Federal Procurement Data,” and Katherine V. Schinasi’s 2005 report “Improvements Needed for the Federal Procurement Data System—Next Generation”).

In addition, FPDS data from past years are continuously updated over time. While FY2007 was long closed, over $100 billion worth of entries for that year were modified in 2010. This explains any discrepancies between the data presented in this report and those in previous editions. The study team changes over prior-year data when a significant change in topline spending is observed in the updates. Tracking these changes does reduce ease of comparison to past years, but the revisions also enable the report to use the best available data and monitor for abuse of updates. 

Despite its flaws, FPDS is the only comprehensive data source of government contracting activity, and it is more than adequate for any analysis focused on trends and order-of-magnitude comparisons. To be transparent about weaknesses in the data, this report consistently describes data that could not be classified due to missing entries or contradictory information as “unlabeled” rather than including it in an “other” category.

The 2016 data used in this report were downloaded in January 2017. The 2017 data used in this report were downloaded in January 2018; a full re-download of all back-year data was performed simultaneously.

**Military aircraft engines** 
The dataset of Aircraft Engine contract transactions was selected by multiple criteria. First, only contracts managed by the Department of Defense and falling in CSIS's Aircraft Platform Portfolio were included in the dataset. The platform portfolio classification looks to whether a transaction supports a specific aircraft platform (based on DoD Acquisition Program), is administered by platform-dedicated agency (i.e. the Missile Defense Agency), is part of a broader category of aircraft programs (based on the Claimant Program Code), or uses a product or service code specific to aircraft. The second step in creating the dataset was limiting specifically to those contracts relevant to engines according to either Claimant Program Code (A1B: Aircraft Engines or Spares) or one of the multiple product or service codes relating to all types of engines (see [contracts\data\EngineProductOrServiceCodes.csv](contracts\data\EngineProductOrServiceCodes.csv)).
 
**Notable contract variables** 

`fy`: the fiscal year for the contract obligation

`customer`: the military customer, which includes Army, Air Force, Navy, DLA, and Other DoD. 

`category`: the type of contract obligation, which includes products, services, and R&D. 

`project`: the name of the project for the contract obligation. 

`parent`: the company receiving the contract. 

`vendor_size`: the size of the company receiving the contract. 

`competition`: the way that the contract was competed. 

`contract_type`: the type of contract. 

`amount`: the dollar value of the contract. 


# 3. budget

The purpose of the budget dataset is to identify important Research Development Testing and Evaluation (RDT&E) investments in military aircraft engines, as well as to compare DoD’s spending plans to its actual spending. 

**Future Years Defense Program methodology**

Most years, DoD releases its Future Years Defense Program, a five-year spending plan for each program, in a set of budget documents. These documents, known as justification books, are available on the DoD comptroller website. Our study team analyzed the justification books from 1999 to 2019 for Army, Navy, and Air Force to identify spending that was directly related to military aircraft engines. 

We began with R-2s (RDT&E documents) and identified relevant program elements based on “Mission Description and Budget Item Justification”. We looked to program elements that mentioned turbine engines or more advanced aerospace technologies such as ramjets or hypersonics. We then identified relevant projects within each program. Each program element is broken down into separate projects. For example, *`Aerospace Propulsion and Power Technology`* had six projects in the 2019 President’s Budget request: *`Aerospace Fuels`*, *`Aerospace Power Technology`*, *`Aircraft Propulsion Subsystems Int`*, *`Space & Missile Rocket Propulsion`*, *`Advanced Aerospace Propulsion`*, and *`Advanced Turbine Engine Gas Generator`*. 

We, once again read the “Mission Description and Budget Item Justification”, this time for each project, and determined which projects were sufficiently relevant to military aircraft engines. For the projects that were, we collected their spending plan and consolidated the numbers into a single database. The project names, and even the project numbers, sometimes changed from year to year. So, the study team also identified such changes and updated the names to accurately reflect the projects in our trend analysis. These changes can be seen within the data_processing.R file. 

**Bugdet variables**

`fydp_year`: the President’s Budget Request Year. For most recent justification books were released for PB 2019.  

`fy`: the fiscal year for relevant spending. For example, the PB 2019 request includes a spending plan for fiscal years 2019, 2020, 2021, 2022, and 2023. 

`account`: the RDT&E budget activity. This includes: basic research, applied research, advanced technology development, advanced component development & prototypes, system development & demonstration, management support, and operational systems development. 

`organization`: the military service, which includes Army, Air Force, and Navy. 

`program_number` and `program_name`: the R-1 Program Element number and name 

`project_number` and `project_name`: the project number and name (a subcategory of the R-1 Program Element). 

**Mission description and budget item justifications**

*`Advanced Aerospace Propulsion`*
This project develops and demonstrates, via ground and flight tests, the scramjet propulsion cycle to a technology readiness level appropriate for full integration with other engine cycles (including turbine and rocket-based) to provide the Air Force with transformational military capabilities. The primary focus is on the hydrocarbonfueled, scramjet engine. Multi-cycle engines will provide the propulsion systems for possible application to support aircraft and weapon platforms operating up to Mach 7. Efforts include: scramjet flow-path optimization to enable operation over the widest possible range of Mach numbers; active combustion control to assure continuous positive thrust (even during mode transition); robust flame-holding to maintain stability through flow distortions; and maximized volume-to-surface area to minimize the thermal load imposed by the high-speed engine. Thermal management plays a vital role in scramjet and combined cycle engines, including considerations for protecting low speed propulsion systems (e.g., turbine engines) during hypersonic flight.

*`Advanced Propulsion Technology`*
This project develops combined/advanced cycle air breathing high-speed (up to Mach 5) and hypersonic (Mach 5 to 7) propulsion technologies to provide revolutionary propulsion options for the Air Force. These new engine technologies will enable future high-speed/hypersonic weapons and aircraft concepts. The primary focus is on hydrocarbon-fueled engines capable of operating over a broad range of flight Mach numbers. Efforts include modeling, simulations, and proof of concept demonstrations of critical components; advanced component development; and ground-based demonstrations.

*`Advanced Turbine Engine Gas Generator`*
This project develops and demonstrates technology to increase turbine engine operational reliability, durability, mission flexibility, and performance while reducing weight, fuel consumption, and cost of ownership. The objective is to provide continuous evolution of technologies into an advanced gas generator in which the performance, cost, durability, repairability, and maintainability can be assessed in a realistic engine environment. The gas generator, or core, is the basic building block of the engine and nominally consists of a compressor, a combustor, a high-pressure turbine, mechanical systems, and core subsystems. Experimental core engine demonstration validates engineering design tools and enhances rapid, low-risk transition of key engine technologies into engineering development, where they can be applied to derivative and/or new systems. These technologies are applicable to a wide range of military and commercial systems including aircraft, missiles, land combat vehicles, ships, and responsive space launch. Component technologies are demonstrated in a core (sub-engine). This project also assesses the impact of low spool components such as; inlet systems, fans, low pressure turbines, exhaust systems, and system level technologies such as; integrated power generators and thermal management systems on core engine performance, and durability in ground demonstrations of engine cores. The core performances of this project are validated on demonstrator engines in the APSI Project of this program. A portion of this project supports the demonstration of adaptive cycle technologies, which develop component technology for an adaptive cycle engine architecture that provides optimized performance, fuel efficiency, and durability for widely varying mission needs.

*`Aerospace Fuels`*
This project evaluates and demonstrates improved hydrocarbon fuels, unique special application fuels, alternate fuels and advanced, novel aerospace propulsion technologies for Air Force applications, including high-speed and hypersonic flight and technologies to increase turbine engine operational reliability, durability, mission flexibility, and performance, while reducing weight, fuel consumption, and cost of ownership. The advanced fuel emphasis is on demonstrating new thermally stable, high-heat sink, and controlled chemically reacting fuels for a conventional turbine engine, turbine-based combined cycle engines, and other advanced propulsion systems. The project also evaluates and demonstrates fuel system components that minimize cost, reduce maintenance, and improve performance of future aerospace systems. The advanced propulsion emphasis is on demonstrating concepts for combined cycle, ramjet, and scramjet engines. A portion of this project supports the demonstration of adaptive cycle technologies. This project develops component technology for an adaptive cycle engine architecture that provides optimized performance, fuel efficiency, and durability for widely varying mission needs.

*`Aircraft Propulsion Subsystems Int`*
This project develops and demonstrates technology to increase turbine engine operational reliability, durability, mission flexibility, and performance while reducing weight, fuel consumption, and cost of ownership. The Aerospace Propulsion Subsystems Integration (APSI) project includes demonstrator engines for manned systems and efficient small-scale propulsion for remotely piloted aircraft and cruise missile applications. The demonstrator engines integrate the core (high- pressure spool) technology developed under the Advanced Turbine Engine Gas Generator (ATEGG) project with the engine (low-pressure spool) technology such as fans, turbines, engine controls, mechanical systems, exhaust nozzles, and augmentors. Additionally, this project includes activities to improve propulsion safety and readiness. This project also focuses on integration of inlets, nozzles, engine-to-airframe compatibility, and power and thermal management subsystems technologies. The APSI project provides aircraft with potential for longer range and higher cruise speeds with lower specific fuel consumption, surge power for successful engagements, high sortie rates with reduced maintenance, reduced life cycle cost, and improved survivability, resulting in increased mission effectiveness. Technologies developed are applicable to sustained high-speed vehicles and responsive space launch. The APSI project is focused on improving propulsion capabilities while at the same time reducing the cost of ownership. Anticipated technology advances include turbine engine improvements providing approximately twice the range for a sustained supersonic combat aircraft, doubling the time on station with ten times the power output for surveillance aircraft and propulsion for a high speed supersonic missile with double the range for time sensitive targets. A portion of this project supports the demonstration of adaptive cycle technologies, which develop component technology for an adaptive cycle engine architecture that provides optimized performance, fuel efficiency, high power extraction, integrated thermal management, and durability for widely varying mission needs.

*`Combustion and Mechanical Systems`*
This project evaluates lubricants, mechanical systems, and combustion concepts for advanced turbine engines, pulse detonation engines, and combined cycle engines. This project also develops technologies to increase turbine engine operational reliability, durability, mission flexibility, maintainability, and performance while reducing weight, fuel consumption, and cost of ownership. Applications include: missiles, aircraft, and re-usable high-speed vehicles. Analytical and experimental areas of emphasis include: lubricants, bearings, mechanical systems diagnostics, mechanical systems prognostics, rotor dynamics, oil-less engine technology, optical diagnostics, fundamental combustion, detonations, combustors, and afterburners. Lubricants for these engines must be thermally stable, cost-effective, and operate over a broad range of conditions. Advanced combustion concepts must be cost-effective, durable, and reduce pollutant emissions. A portion of this project supports adaptive cycle technologies. This effort develops component technology for an adaptive cycle engine architecture that provides both optimized performance and fuel efficiency for widely varying mission needs.

*`Materials for Structures, Propulsion, and Subsystems`*
This project develops the materials and processing technology base for aircraft, spacecraft, launch systems, and missiles to improve affordability, maintainability, and performance of current and future Air Force systems. A family of affordable lightweight materials is being developed, including metals, polymers, ceramics, metallic and nonmetallic composites, and hybrid materials to provide upgraded capabilities for existing aircraft, missile, and propulsion systems to meet the future system requirements. The project develops high-temperature turbine engine materials that will enable engine designs to double the turbine engine thrust-to-weight ratio. Advanced high temperature protection materials are being developed that are affordable, lightweight, dimensionally stable, thermally conductive, and/or ablation and erosion resistant to meet aerospace and missile requirements. Alternative or replacement materials are being developed to maintain the performance of aging operational systems. Materials for thermal management including coolants, adaptive thermally conductive materials, coatings, friction and wear-resistant materials, and other pervasive nonstructural materials technologies are being developed for directed energy, propulsion, and subsystems on aircraft, spacecraft, and missiles. The project concurrently develops advanced processing methods to enable adaptive processing of aerospace materials.

*`Turbine Engine Technology`*
This project develops technology to increase turbine engine operational reliability, durability, mission flexibility, and performance, while reducing weight, fuel consumption, and cost of ownership. Analytical and experimental areas of emphasis are fans and compressors, high temperature combustors, turbines, internal flow systems, controls, augmentor and exhaust systems, integrated power and thermal management systems, engine inlet integration, mechanical systems, adaptive cycle technologies, and structural design. This project develops component technology for an adaptive cycle engine architecture that provides both optimized performance and fuel efficiency for widely varying mission needs. This project supports joint DoD, agency, and industry efforts to focus turbine propulsion technology on national needs. The program plan is relevant across capability areas for global responsive strike, tactical and global mobility, responsive space lift, and persistent intelligence, surveillance, and reconnaissance (ISR).

*`Aircraft Engine Component Improvement Program (USAF)`* 
The Aircraft Engine Component Improvement Program (CIP) provides the only source of critical sustaining engineering support for in-service Air Force engines to maintain flight safety (highest priority) to correct deficiencies, improve system operational readiness (OR) and reliability & maintainability (R&M), reduce engine Life Cycle Cost (LCC), and sustain engines throughout their service life. Changes in aircraft operational parameters caused by changing missions and tasks accelerate new engine problems; Engine CIP provides the means to develop fixes for these problems. Engine CIP funding is driven by field events and types/maturity of engines, not by the total engine quantity. The program starts with government acceptance of the first procurement-funded engine and continues over the engine's life, gradually decreasing to a minimum level (safety/depot repairs) sufficient to keep older engines operational. Engine CIP testing identifies and fixes engine-related problems ahead of operational impacts. R&M related Engine CIP efforts significantly reduce out year Operations and Maintenance (O&M) and spares costs. This program is in Budget Activity 7, Operational System Development, because this budget activity includes development efforts to upgrade systems that have been fielded or have received approval for full rate production and anticipate production funding in the current or subsequent fiscal year.

*`Aircraft Engine Component Improvement Program (F135)`* 
The F135 Aircraft Engine Component Improvement Program (CIP) supports F-35 single-engine fighter propulsion system. It provides the only source of critical developmental engineering support for the F135 propulsion system. F135 CIP maintains flight safety (highest priority), corrects service revealed deficiencies, improves system Operational Readiness (OR) and Reliability & Maintainability (R&M), reduces propulsion system Life Cycle Cost (LCC), and sustains the propulsion system throughout its service life. Historically, aircraft systems change missions, tactics, and environment (including new fuels) and meet changing threats throughout their lives. New technical problems can develop in the propulsion system through actual use and the F135 CIP provides the means to develop fixes for these problems. F135 CIP funding is driven by field events and type/maturity of the propulsion system, not by the total quantity of engines. The program starts with government acceptance of the first procurement-funded engine and continues over the propulsion system's life, gradually decreasing to a minimum level (safety/depot repairs) sufficient to keep older engines operational. F135 CIP, through "Lead the Fleet" operational use and accelerated mission testing, identifies and fixes propulsion-related problems ahead of operational impacts. F135 CIP ensures continued improvements in R&M, which reduce out year support costs. Historically, R&M related CIP efforts significantly reduce out year O&M and spares costs.

*`AV-8B`*
This program provides for AV-8B Design, Development, Integration and Test of the following improvements: The Engine Life Management Program (ELMP), Operational Flight Program (OFP) and Avionics/Weapons Integration, Escape System, and Readiness Management Plan (RMP). The ELMP is a comprehensive plan to increase safety of flight and operational readiness of the AV-8B F402-RR-408 Engine and Gas Turbine Starter, as well as other critical engine components. The Program Office will accomplish this mission through the Component Improvement Program, which entails Engineering Project Description investigations to derive safety and reliability improvements to the engine and engine components. The Joint Mission Planning System is required as part of the Department of Navy directed migration to a common Navy and Marine Corps mission planning system. H6.2 provides Global Positioning System navigation capabilities, a Litening common OFP update and initial Link 16 capability to include use of the APX-123, initial Mode 5 capability, as well as software updates. H7.0 OFP will integrate full Harrier Link 16 capability and provide software updates. H7.0 will also integrate AIM-9X, a Litening Common OFP update, provide Advanced Precision Kill Weapon System (APKWS) integration improvements, Joint Standoff Weapons (JSOW), and common avionics ADS-B (out), Mode 5, and Mode S Identification Friend or Foe capabilities as well as integrate required Radar Display Computer processing improvements to enable H7.0 functionality. Other specific efforts include peculiar integration and flight test requirements such as AIM-120C flight test, as AIM-120A/B will become obsolete, as well as AIM-120 mixed stores flight test, unique weapons, sensors, and countermeasures integration and stores expansion to include APKWS, Helmet Mounted Cueing System (HMCS), AIM-9X, ALE-43, standoff weapons such as Joint Standoff Weapons (JSOW) and unique flight test of other avionics, sensors, or weapons systems , or emergent tactical requirements, as they arise. The program is working closely with the Common Avionics Program and the Allies (Spain and Italy) on all efforts. RMP represents all engineering activities for development, design and test to support aircraft safety, flight clearance and concept exploration for resolution of emergent safety, service life, escape systems, compatibility, obsolescence, and readiness issues as well as response to fleet urgent operational requirements.

*`Aircraft Engine Component Improvement Program (USN)`* 
The Propulsion and Power (P&P) Component Improvement Program (CIP) provides the only source of critical design and development engineering support to resolve safety, reliability and maintainability deficiencies of in-service Navy and Marine Corps aircraft propulsion systems. The highest priority issues P&P CIP addresses concern safety-of-flight deficiencies, which account for approximately 80% of P&P CIP efforts. The program also corrects service-revealed deficiencies, improves Operational Readiness and Reliability and Maintainability, and reduces platform Life Cycle Cost. Budgets are allocated across platform-specific teams and multi-platform product support teams based upon long term strategies to achieve safety and affordable readiness goals; the R-3 exhibit details annual portions of those long-term strategies. P&P CIP tasks have reduced the rate of in-flight aborts, safety incidents, non-mission capable rates, scheduled and unscheduled engine removals, maintenance work hours, and overall cost of ownership. This is accomplished through the maintenance and validation of specification performance, testing to qualify engineering changes, verifying life limits, and improving the inherent reliability of the propulsion and power systems as an integral part of Reliability Centered Maintenance initiatives. Historically, the missions, tactics, and environmental exposure of military aircraft systems change to meet new threats or operational demands, and often result in unforeseen problems, which if not corrected, can cause critical safety/readiness degradation, such as those experienced during OPERATIONS DESERT SHIELD/DESERT STORM, ENDURING FREEDOM, and IRAQI FREEDOM due to sand erosion. In addition, new problems arise through actual fleet deployment and usage of the aircraft. System development programs, while geared to resolve as many problems as possible before deployment, cannot duplicate actual operations or account for the vast array of environmental and usage variables, particularly when aircraft missions vary from those that the aircraft was designed to perform. Therefore, it has been found that P&P CIP can provide an immediate engineering response to these flight-critical problems and accelerated engine testing can avoid potential problems. P&P CIP starts after development and Navy acceptance of the first production article and addresses usage and life problems not covered by warranties. P&P CIP addresses engines, transmissions, propellers, starters, auxiliary power units, electrical generating systems, aircraft wiring, and fuel and lubricant systems. These efforts continue over the system's life, gradually decreasing to a minimum level sufficient to maintain the reliability, and decrease the operating costs, of older inventory. P&P CIP is a highly leveraged and cooperative tri-service program with Foreign Military Sales participation.

*`ACFT Demo Engines`* 
This Project matures and demonstrates power system technologies through design, fabrication, and evaluation of advanced engine components in order to improve the performance of turbine engines and drive systems for vertical lift aircraft and Unmanned Aerial Systems (UAS) vehicles This Project supports Army modernization by demonstrating mature technologies for lighter turbine engines and drives that provide increased power, increased fuel efficiency, improved sustainability and reduced maintenance. These advanced engine designs and drives will significantly improve the overall aircraft performance characteristics and reduce the logistical footprint of Army Aircraft.

*`Veh Prop & Struct Tech`*
This Project investigates engine, drive train, and airframe enabling technologies such as multifunctional materials, fluid mechanics and high temperature, high strength, low cost shaft materials. Additional areas of research include platform, aerodynamic, transmission, and control technologies for implementation in autonomous Unmanned Aerial Systems (UAS) and failure analysis and prediction models and techniques to support a "zero maintenance helicopter" concept. Work in this Project complements and is fully coordinated with Program Element (PE) 0603003A (Aviation Advanced Technology) and leverages basic research performed in PE 0601104/Project H54 (Micro Autonomous Systems Technology Collaborative Technology Alliance) and PE 0601104/Project H09 (Robotics Collaborative Technology Alliance).

*`Adv Propulsion Rsch`* 
This Project fosters research to increase the performance of small air-breathing engines and power-trains to support improved system mobility, reliability, and survivability for air and/or ground vehicles; and ultimately serves to reduce the logistics cost burden for the future force. Problems addressed include the need for greater fuel efficiency and reduced weight in these propulsion systems. Technical barriers to advanced propulsion systems are the inadequacy of existing materials to safely withstand higher temperature demands, the lack of capability to accurately simulate the flow physics and the mechanical behavior of these systems, including the engine and drive train. The Army is the lead Service in these technology areas and performs basic research in propulsion, as applicable to rotorcraft as well as tracked and wheeled vehicles. Technical solutions are being pursued through analysis, code generation, and evaluations to improve engine and drive train components and investigate advanced materials. Component level investigations include compressors, combustors, turbines, energy sources and conversion, injectors, pistons, cylinder liners, piston rings, gears, seals, bearings, shafts, and controls. Work in this Project provides the technical underpinnings for Program Element (PE) 0602211A (Aviation Technology).

*`Aircraft Engine Component Improvement Program`*
Aircraft Engine Component Improvement Program (CIP) develops, tests, and qualifies improvements to aircraft engine components to correct service-revealed deficiencies, improve flight safety, enhance readiness and reduce operating and support (O&S) costs. In addition, CIP provides the test vehicles for the testing and qualification efforts required as a part of the Army's Critical Safety Item (CSI) program. Non-program specific Auxiliary Power Unit (APU) as well as Unmanned Aerial Vehicle (UAV) safety and readiness issues are also addressed under this Program Element.

*`Improved Turbine Engine Program`*
ITEP develops, tests, qualifies, and integrates the next generation turboshaft engine on the Black Hawk and Apache aircraft. The Improved Turbine Engine (ITE) replaces the existing T700 engine design originated in the 1970's and meets the operational requirement of 6,000 feet pressure altitude and 95 degrees (6K/95). The ITE will fit inside the existing engine bays of the Black Hawk and Apache Helicopters and provides a significant power enhancement of up to fifty percent (total of 3,000 class shaft horsepower) with increased fuel efficiency. Additional benefits include improved design life, enhanced reliability, lower maintenance cost and restored capability lost due to aircraft weight growth, without increasing the logistics footprint. The program consists of systems engineering and program management, detailed design engineering, design assurance, hardware manufacturing and testing, component and module level development and testing, system level testing and qualification, as well as integration into the airframe.

*`F135`*
F135 Propulsion System SDD execution of the F135 Propulsion System (Pratt & Whitney). 

*`F136`*
F136 Propulsion System SDD execution of the F136 Propulsion System (General Electric). 