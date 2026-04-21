# Medicaid Provider Spending

The [US Department of Health & Human Services](https://www.hhs.gov/) (HHS) recently released launched an open data initiative to make datasets available to the public (https://opendata.hhs.gov/). As of now, there is one (1) dataset available released on February 9, 2026: [_HHS Medicaid Provider Spending by HCPCS_](https://opendata.hhs.gov/datasets/medicaid-provider-spending/).

This dataset provides _monthly_ spending for outpatient and professional services billed to [Medicaid](https://www.medicaid.gov/) across the United States from 2018-2024 by provider (billing + servicing) and procedure ([HCPCS](https://www.cms.gov/medicare/coding-billing/healthcare-common-procedure-system)) code.

## Setup

The dataset was downloaded from the [webpage](https://opendata.hhs.gov/datasets/medicaid-provider-spending/) as a [DuckDB](https://duckdb.org/) database. As of the most recent download on 4/21/2026, the file is ~3.5 GB, thus was not pushed to the GitHub repo. To enable code in this repository to run, you should download the database and place it in your local project directory. It was also downloaded as a [Parquet](https://parquet.apache.org/) file as an alternative means of access.
