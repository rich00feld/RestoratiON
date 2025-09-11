# RestoratiON

Ontario Ministry of Natural Resources (OMNR) Landscape Ecology restoration prioritization project. This tool aims to identify degraded land areas within a chosen geographic region and determine which areas, when restored to nearby natural habitat types, will provide the greatest benefit to biodiversity based on user-selected criteria. For more information about the tool or to try it out, users can access it here: [RestoratiON](#0). Documentation is available within the website (links here: [background](#0), [data sources](#0), and [glossary](#0)); and step-by-step instructions are available as you progress through the tool. There is also a [video tutorial](#0) available (accessible version here: [described video version](#0)).

## Prerequisites

We provide instructions on how to run the tool in a Windows environment. This tool has not been tested on Mac or Linux. In order to run the tool locally on your own machine you first need to download and install:

1.  **R:** <https://cran.rstudio.com/> (Click 'Download R for Windows' and install to your machine).
2.  **Rtools (Windows):** <https://cran.rstudio.com/bin/windows/Rtools/> (Download RTools 4.4).
3.  **RStudio:** <https://posit.co/download/rstudio-desktop/> (Click 'Download RStudio Desktop for Windows').

### Get the Base Files

4.  Download the base files that contain the spatial data to run the tool from:\
    <https://sobr.ca/wp-content/uploads/RestoratiON-App.zip>\
    Unzip this folder on your local machine; note where you've placed the folder.

5.  To ensure you are using the most up-to-date version of the tool's scripts, download the `app.R` file from this repository and place it in the base directory you just unzipped, overwriting the existing `app.R` in the folder.\
    [Go to](https://github.com/rich00feld/RestoratiON/blob/main/app.R)[app.R](https://github.com/rich00feld/RestoratiON/blob/main/app.R)

    After you have navigated to the app.R page in GitHub, find the 'download raw file' icon at the upper left of the page. It looks like a downward arrow pointing towards an open square box.

    ![App Folder](images/download-app.png)

    After downloading app.R, replace the app.R file in the "Restoration App" folder you downloaded and unzipped in step 4. This ensures that you are using the most up-to-date script.

    ![App Folder](images/app-folder.png)

------------------------------------------------------------------------

## Running the Tool

6.  Open `app.R` in RStudio and press **CTRL + SHIFT + ENTER** or alternatively click **Run App** in the top-right of the script pane.

    ![Run App](images/run-app.png)

The tool will initialize and may ask if you want to install the **Shiny** package, click yes.

![Shiny Yes](images/shiny-yes.png)

When the tool first runs it may take a while to install all the dependent packages, but this will only occur once in a given installation, and not occur if the tool is re-run. If you experience errors, sometimes closing RStudio and repeating step 6 can resolve issues.

If you experience package installation prompts when you first run the app, please wait. Let R install all required packages; subsequent runs of the tool will be much faster.

*If you have issues during this step, or errors occur while working with the tool, please see the final paragraph in this README: **Troubleshooting.***

------------------------------------------------------------------------

### Optional: Set the App Run Mode

The tool has the option to run calculations **sequentially** (slower but less memory intensive) or in **parallel** (faster but more memory intensive). By default, the tool is set to run **sequentially**. If your system has multiple CPU cores and sufficient RAM, the tool can use parallelization instead. If you're not sure, it's fine to leave the app settings at their default values.

7.  To enable parallel mode, change the integer at `run_mode <- reactiveVal(1)` to `2` so it reads:

``` r
# set up reactive values
run_mode <- reactiveVal(2)  # 1 = sequential, 2 = parallel
```

> Tip: Use **Ctrl + F** in the editor to search for `run_mode <- reactiveVal`.

Then save your changes (**Ctrl + S**) before running the app.

-   If you want to switch back to sequential mode, set `run_mode <- reactiveVal(1)`.

------------------------------------------------------------------------

### Optional: Set the Number of Parallel Workers

You can control the number of workers (CPU cores) used in parallel mode. Two workers were most stable on the systems tested, but you may increase this if you have sufficient RAM and CPU cores.

-   A general rule of thumb is **RAM (GB) ÷ 8 ≈ workers**\
    e.g., 16 GB RAM → 2 workers; 32 GB RAM → 4 workers.\
    (This may vary depending on your CPU.)

8.  To set the number of workers, change the integer at `set_cores <- reactiveVal(2)` to a suitable value for your stystem (the example below uses 4):

``` r
# set the number of parallel workers
set_cores <- reactiveVal(4)
```

Then save your changes (**Ctrl + S**) before running the tool.

------------------------------------------------------------------------

## Troubleshooting

*If you experience issues with step 6 (Running the Tool) - either starting the tool, or while using the tool, please read this section, below.*

**For users who have some experience with R and want to run this tool:**

-   During the tool’s inception, we explored using `renv` to save appropriate package versions and load them with the tool. However, this approach was error-prone and required an inexperienced user to troubleshoot difficult issues. It also required us to package over 1.3 GB of package data along with the tool, which was over the size limit we have available on our host website.

-   Instead, we provide code within the tool (follow steps below) to install (and/or upgrade and downgrade) package versions that were used to create the tool. Installing these versions will ensure that the tool runs smoothly and doesn’t encounter errors due to package upgrades breaking the tool.

    -   **Please be aware that if you choose to run the code using the steps above, libraries currently installed on your machine may be upgraded or downgraded. You may wish to note what versions of each package you have so that you can re-download your desired version as necessary. Read the next section for more details and to find out what package versions will be installed.**

-   If you follow the steps below and still have issues while running the app, it could be that you already had a package version installed that is either older, or newer, than what is required by this tool. You may have to ensure that each package used by the tool is the version listed in the app.R file (see below).

**For users who are new to R and have just downloaded R and RStudio to use this tool:**

-   Ensure you are running RStudio as an administrator.

-   Inside the app.R file in RStudio, scroll down slightly in the file to look for the text `# #Version control script contains last confirmed working package versions….`. This should be about 2 ‘paragraphs’ down.

    -   Copy the text, *including the two hashtags*, from that line down to the line immediately before the text `# Load libraries ----`. It should be about a page and a third of text.

    -   Go to the File tab (top menu bar) and press ‘New File’ and choose “R Script”.

    -   Save the file – call it ‘install libraries.R’. Paste the text you just copied into this file, and save again.

-   Under the Session tab (top menu bar) click ‘Clear Workspace…’ and choose ‘Yes’.

-   Under the Session tab (top menu bar) click ‘Restart R’.

-   Now, select all the copied text in the ‘install libraries.R’ file.

    -   Under the Code tab (top menu bar) click “Comment/Uncomment lines”.

-   Select all the code in the file and click the “Run” button at the top right of the file. R will try to download and install the proper versions of the packages required to run the app.

-   If you see a pop-up that asks you if you want to restart R prior to install, select **No.**

-   If you get an error message in the 'Console' window that a particular package did not install, you can install it manually. For example, if 'RcppEigen' did not install, look for the proper version in the app.R file, under the section '\# \# Desired package versions'.

    -   In the console, type `remotes::install_version("RcppEigen", version = "0.3.4.0.2", upgrade = "never", type = "source")` and then press Enter to run the line of code.

    -   Continue to install any packages that had errors in this manner.

    -   To check if all required packages are installed, you may re-run the code in the 'install libraries.R' file; it will skip packages you already installed, and let you know if any require more attention.

    -   It is a good idea to close RStudio after this step and start with a clean session before trying to run the app.

-   After restarting RStudio, go back to the ‘app.R’ file and select ‘Run App’.

------------------------------------------------------------------------
