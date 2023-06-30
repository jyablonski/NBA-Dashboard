# [NBA-Dashboard](https://jyablonski.shinyapps.io/nbadashboard)

# ![Tests](https://github.com/jyablonski/NBA-Dashboard/actions/workflows/deploy.yml/badge.svg)

## About
* This Project consists of an [R Shiny Dashboard](https://nbadashboard.jyablonski.dev) displaying recent NBA data, analyses, and trends, along with all of the processes behind the ELT Pipeline that supports it.

## ELT Data Pipeline Workflow

![NBA ELT Pipeline Data Flow 2](https://github.com/jyablonski/NBA-Dashboard/assets/16946556/fd267c49-7794-4625-9845-7d3b4c37dd1e)

1. NBA Data is web scraped in Python on a Cron Schedule ran via ECS Fargate, and data is subsequently stored to source tables in a PostgreSQL Database.
2. dbt Cloud executes data transformations in SQL on a Cron Schedule following the ECS Task, and also performs automated schema testing, quality checks, and data validation assertions primarily via [dbt_expectations](https://github.com/calogica/dbt-expectations).
3. An ML Pipeline is then run in Python via ECS Fargate to predict Team Win %s for Upcoming Games that night.
4. AWS Step Functions is used as an orchestration tool to run all 3 tasks in sequence.
5. The Shiny Server is built & deployed to [ECS](https://nbadashboard.jyablonski.dev) where it queries from the transformed SQL tables to display current stats, player metrics, gambling odds, and upcoming schedule data.

* Links to other Repos providing infrastructure for this Project

	* [Ingestion Script](https://github.com/jyablonski/python_docker)
	* [Terraform](https://github.com/jyablonski/aws_terraform/)
	* [dbt](https://github.com/jyablonski/nba_elt_dbt)
	* [ML Pipeline](https://github.com/jyablonski/nba_elt_mlflow)
	* [Airflow Proof of Concept](https://github.com/jyablonski/nba_elt_airflow)
	* [REST API](https://github.com/jyablonski/nba_elt_rest_api)
  
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
	* tweepy
	* scikit-learn
	* pytest