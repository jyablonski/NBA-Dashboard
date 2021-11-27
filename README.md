# [NBA-Dashboard](https://jyablonski.shinyapps.io/nbadashboard)

# ![Tests](https://github.com/jyablonski/NBA-Dashboard/actions/workflows/deploy.yml/badge.svg)

## Design Workflow

![11-9-21nbaflow](https://user-images.githubusercontent.com/16946556/141023947-e6b879c3-24c3-400a-abb2-77e75dbf1dc2.jpg)

* This Personal Project consists of an [R Shiny Dashboard](https://jyablonski.shinyapps.io/nbadashboard) displaying recent NBA data, analyses, and trends, along with all of the processes behind the ELT Pipeline that supports it.

	1. NBA Data is web scraped in Python on a Cron Schedule ran via ECS Fargate, and data is subsequently stored to source tables in a PostgreSQL Database.
	2.  dbt Cloud executes data transformations in SQL on a Cron Schedule following the ECS Task, and also performs automated schema testing & data validation checks primarily via [dbt_expectations](https://github.com/calogica/dbt-expectations).
	3. The Shiny Server is built & deployed to [shinyapps](https://www.shinyapps.io) and queries from the transformed SQL tables to display current stats, player metrics, gambling odds, and upcoming schedule data.

		* Any ELT Script failure or dbt Cloud Error / testing failure triggers an automatic email alert detailing the error(s).
		* AWS Infrastructure is constructed via Terraform which includes an ECR Repository, an RDS PostgreSQL DB, and an ECS Task to run the ELT Script, as well as all of the supporting architecture needed for those services (IAM Roles, Lifecycle Policies, Cloudwatch Logs, Security Groups, VPCs/Subnets etc).
		* GitHub Actions are utilized to build CI/CD Workflows for:
			*  Automated testing with Pytest & deployment to Coveralls for code coverage
			*  The Python Script to build the Docker Image & push to ECR
			*  The Server code to build the Shiny Application & push to [shinyapps.io](https://www.shinyapps.io/).

  
* Links to other Repos providing infrastructure for this Project

	* [Python Web Scrape Script](https://github.com/jyablonski/python_docker)
	* [Terraform](https://github.com/jyablonski/aws_terraform/tree/master/prod)
	* [dbt](https://github.com/jyablonski/nba_elt_dbt)
	* [Airflow Proof of Concept](https://github.com/jyablonski/nba_elt_airflow)
		* Ideally I would use this for workflow orchestration, but I cannot host a stable version of Airflow for free to my knowledge.
		* This Repo utilizes a Docker template to work with Airflow locally, and I've created QA & Prod DAGs to resemble what a production Airflow workflow would look like for this project.

  
* Main R Packages Used

	* Shiny
	* Tidyverse
	* Plotly
	* GT
	* renv

  
* Main Python Packages Used

	* Pandas
	* BeautifulSoup4
	* SQLAlchemy
	* Boto3
	* PRAW