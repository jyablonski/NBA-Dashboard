# [NBA-Dashboard](https://jyablonski.shinyapps.io/nbadashboard)

# ![Tests](https://github.com/jyablonski/NBA-Dashboard/actions/workflows/deploy.yml/badge.svg)

## About
* This Personal Project consists of an [R Shiny Dashboard](https://jyablonski.shinyapps.io/nbadashboard) displaying recent NBA data, analyses, and trends, along with all of the processes behind the ELT Pipeline that supports it.

## ELT Data Pipeline Workflow

![NBA ELT Pipeline Data Flow 2 (5)](https://user-images.githubusercontent.com/16946556/169709299-6cbc6722-e15c-468a-8802-233ae46819f0.jpg)

1. NBA Data is web scraped in Python on a Cron Schedule ran via ECS Fargate, and data is subsequently stored to source tables in a PostgreSQL Database.
2. dbt Cloud executes data transformations in SQL on a Cron Schedule following the ECS Task, and also performs automated schema testing, quality checks, and data validation assertions primarily via [dbt_expectations](https://github.com/calogica/dbt-expectations).
3. An ML Pipeline is then run in Python via ECS Fargate to predict Team Win %s for Upcoming Games that night.
4. The Shiny Server is built & deployed to [shinyapps](https://www.shinyapps.io) where it queries from the transformed SQL tables to display current stats, player metrics, gambling odds, and upcoming schedule data.
   * Any ELT Script failure or dbt Cloud Error / testing failure triggers an automatic email alert detailing the error(s).
   * **100%** of AWS Infrastructure is constructed via Terraform with the primary services being an ECR Repository, an RDS PostgreSQL DB, and an ECS Task to run the ELT Script, as well as all of the supporting architecture needed for those services (IAM Roles, Lifecycle Policies, Cloudwatch Logs, Security Groups, CIDR Block whitelisting, VPC/Subnets etc).
   * GitHub Actions are utilized to build CI/CD Workflows for:
     	*  Automated testing with Pytest & deployment to [Coveralls](https://coveralls.io/) for code coverage.
     	*  The Web Scrape Script to construct the Docker Image & push to the private ECR Repository.
     	*  The Shiny Server code to construct the Shiny Application & push to [shinyapps.io](https://www.shinyapps.io/).
     	*  Terraform Cloud is used to store state & trigger Terraform Plan on any PR, and Terraform Apply after any PR Merge.

    * dbt Cloud offers a free CI Service that uses webhooks to trigger a [temporary build](https://docs.getdbt.com/docs/dbt-cloud/using-dbt-cloud/cloud-enabling-continuous-integration) of the project on any PR to the Project Repo to check for errors that might have been introduced in the proposed changes before they're applied to the Production branch.

  
* Links to other Repos providing infrastructure for this Project

	* [Python Web Scrape](https://github.com/jyablonski/python_docker)
	* [Terraform](https://github.com/jyablonski/aws_terraform/)
	* [dbt](https://github.com/jyablonski/nba_elt_dbt)
	* [ML Pipeline](https://github.com/jyablonski/nba_elt_mlflow)
	* [Airflow Proof of Concept](https://github.com/jyablonski/nba_elt_airflow)
	* [GraphQL API](https://github.com/jyablonski/graphql_praq)
  
* Main R Packages Used

	* Shiny
	* Tidyverse
	* Plotly
	* GT
	* renv

  
* Main Python Packages Used

	* pandas
	* BeautifulSoup4
	* SQLAlchemy
	* Boto3
	* PRAW
	* NLTK
	* twint
	* scikit-learn
	* MLFlow
	* pytest