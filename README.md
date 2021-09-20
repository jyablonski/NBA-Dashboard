# [NBA-Dashboard](https://jyablonski.shinyapps.io/nbadashboard)

# ![Tests](https://github.com/jyablonski/NBA-Dashboard/actions/workflows/deploy.yml/badge.svg)

## Design Workflow

![NBA Project Data Flow (4)](https://user-images.githubusercontent.com/16946556/133896282-f240f145-400e-456d-aec3-4596148eee62.jpg)

1. This Personal Project consists of an [R Shiny Dashboard](https://jyablonski.shinyapps.io/nbadashboard) I've created & all of the processes behind the ETL Pipeline that supports it.
    * NBA Data is webscraped on a Cron Schedule in Python and stored to a SQL DB.
    * AWS Infrastructure is constructed via Terraform to create an ECR Repository, an RDS MySQL DB, and an ECS Task to run the ETL Script, as well as all of the supporting architecture needed for those services (IAM Roles, Lifecycle Policies, Cloudwatch Logs, Security Groups etc).
    * GitHub Actions are utilized to build a CI/CD Workflow to automatically build & push the Dockerfile to ECR, and build & push the Shiny Server Code to [shinyapps.io](https://www.shinyapps.io/) on any Git Push.
    * The Shiny Server connects to the SQL DB and displays current stats, player metrics, gambling odds, and upcoming schedule data.
    * Any ETL Script failure triggers an automatic email alert detailing the error(s).

2. Links to other Repos providing infrastructure for this Project
    * [Python ETL + Dockerfile](https://github.com/jyablonski/python_docker)
    * [Terraform](https://github.com/jyablonski/aws_terraform/tree/master/prod)

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

