# [NBA-Dashboard](https://jyablonski.shinyapps.io/nbadashboard)

# ![Tests](https://github.com/jyablonski/NBA-Dashboard/actions/workflows/deploy.yml/badge.svg)

## Design Workflow

![NBA Project Data Flow](https://user-images.githubusercontent.com/16946556/136297794-e4b4bc77-9980-446c-b5f2-917d2d76aa21.jpg)


1. This Personal Project consists of an [R Shiny Dashboard](https://jyablonski.shinyapps.io/nbadashboard) displaying recent NBA data, analyses, and trends, along with all of the processes behind the ELT Pipeline that supports it.
    * NBA Data is webscraped on a Cron Schedule in Python and stored to a SQL DB.
    * AWS Infrastructure is constructed via Terraform to create an ECR Repository, an RDS PostgresQL DB, and an ECS Task to run the ELT Script, as well as all of the supporting architecture needed for those services (IAM Roles, Lifecycle Policies, Cloudwatch Logs, Security Groups, Public Subnets etc).
    * GitHub Actions are utilized to build CI/CD Workflows to automatically build & push the Docker Image to ECR, and build & push the Shiny Server Code to [shinyapps.io](https://www.shinyapps.io/).
    * DBT Performs data transformations in SQL while also performing automated schema testing + data validation checks via [dbt_expectations](https://github.com/calogica/dbt-expectations).
    * The Shiny Server connects to the transformed SQL tables and displays current stats, player metrics, gambling odds, and upcoming schedule data.
    * Any ELT Script failure triggers an automatic email alert detailing the error(s).
    * Version control has been automated, with the version number & tag being updated on a Git Push to any of the repos.

2. Links to other Repos providing infrastructure for this Project
    * [Python Dockerfile](https://github.com/jyablonski/python_docker)
    * [Terraform](https://github.com/jyablonski/aws_terraform/tree/master/prod)
    * [DBT](https://github.com/jyablonski/nba_elt_dbt)

3. Main R Packages Used
    * Shiny
    * Tidyverse
    * Plotly
    * GT
    * renv

4. Main Python Packages Used
    * Pandas
    * BeautifulSoup4
    * SQLAlchemy
    * Boto3
    * PRAW

