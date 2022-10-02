USE CSIS360
GO


--For instructions see https://github.com/CSISdefense/DIIGsql/blob/master/Doc/Output_Large_Dataset.md


--1h31 m
--2h15m at 47%
SET ANSI_WARNINGS OFF;
SET NOCOUNT ON;
  select  isnull(trim(descriptionofcontractrequirement),'Unlabled'),PlatformPortfolioRemote,sum(obligatedamount) as obligatedamount
  from contract.FPDSpartial
  where fiscal_year >= 2000
  group by isnull(trim(descriptionofcontractrequirement),'Unlabled'),PlatformPortfolioRemote
  
