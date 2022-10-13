USE CSIS360
GO


--For instructions see https://github.com/CSISdefense/DIIGsql/blob/master/Doc/Output_Large_Dataset.md



SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--2H17,m	674,730 rows
exec Project.[SP_EngineAllVendorHistoryCompetitionFundingMechanismVendorSizeProdServAreaSubCustomer]


SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--3H09m	298,478 rows  664,393
exec Summary.SP_CompetitionVendorSizeHistoryBucketPlatformSubCustomer
@Customer='Defense'
,@SubCustomer=NULL
,@PlatformPortfolio=NULL



SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
--6H02m	298,478 rows
exec Vendor.sp_EntityCountHistoryEngineSubCustomer
@Customer='Defense'

