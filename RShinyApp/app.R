library(shiny)
library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(caret)
library(randomForest)
library(cluster)
library(Rtsne)

ui <- fluidPage(
    titlePanel(div(
        style = "text-align: center; color: #FFFFFF; background-color: #154D57; padding: 20px; border-radius: 10px;",
        "Εφαρμογή Ανάλυσης Δεδομένων"
    )),
    tags$head(
        tags$style(HTML("
            body {
                background-color: #FEFAF7;
                font-family: 'Arial', sans-serif;
                color: #000000;
            }
            .sidebar {
                width: 300px;
                background-color: #FFFFFF;
                padding: 15px;
                border-radius: 10px;
                box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
            }
            .main {
                width: calc(100% - 320px);
                margin-left: 320px;
                padding: 15px;
            }
            .btn-file {
                background-color: #154D57;
                border-color: #154D57;
                color: #FFFFFF;
                transition: background-color 0.3s, border-color 0.3s;
            }
            .btn-file:hover {
                background-color: #B7A08B;
                border-color: #B7A08B;
            }
            h4 {
                color: #154D57;
                margin-top: 20px;
            }
            p {
                color: #000000;
                margin-bottom: 10px;
            }
            .form-group {
                margin-bottom: 15px;
            }
            .radio {
                margin-bottom: 15px;
            }
            .plot-container {
                background-color: #FFFFFF;
                padding: 20px;
                border-radius: 10px;
                box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
                margin-bottom: 20px;
            }
        "))
    ),
    sidebarLayout(
        sidebarPanel(
            class = "sidebar",
            fileInput("file", "Επιλέξτε CSV ή Excel Αρχείο", accept = c(".csv", ".xlsx")),
            radioButtons("tabs", "Navigation", choices = c("2D Visualization", "Exploratory Data Analysis", "Comparison", "Info"), inline = TRUE)
        ),
        mainPanel(
            class = "main",
            uiOutput("content")
        )
    )
)

server <- function(input, output, session) {
    data <- reactive({
        req(input$file)
        ext <- tools::file_ext(input$file$name)
        if (ext == "csv") {
            read_csv(input$file$datapath)
        } else if (ext == "xlsx") {
            read_excel(input$file$datapath)
        } else {
            stop("Unsupported file type")
        }
    })

    output$content <- renderUI({
        if (input$tabs == "2D Visualization") {
            tagList(
                radioButtons("viz_algo", "Επιλέξτε Αλγόριθμο που επιθυμείτε να Απεικονήσετε", choices = c("PCA", "t-SNE"), inline = TRUE),
                div(class = "plot-container", plotlyOutput("viz_plot"))
            )
        } else if (input$tabs == "Exploratory Data Analysis") {
            tagList(
                radioButtons("eda_type", "Επιλέξτε EDA Plot Type", choices = c("Histograms", "Boxplots"), inline = TRUE),
                uiOutput("eda_plots")
            )
        } else if (input$tabs == "Comparison") {
            tagList(
                h4("Classification Comparison"),
                sliderInput("reg_param_lr", "Regularization Parameter (C) for Logistic Regression", min = 0.01, max = 10, value = 1),
                sliderInput("num_estimators_rf", "Number of Estimators for Random Forest", min = 1, max = 100, value = 10),
                h4("Clustering Comparison"),
                sliderInput("num_clusters_km", "Number of Clusters (k) for K-Means", min = 2, max = 10, value = 5),
                sliderInput("num_clusters_hc", "Number of Clusters for Hierarchical Clustering", min = 2, max = 10, value = 5),
                div(class = "plot-container", plotlyOutput("comparison_plot"))
            )
        } else if (input$tabs == "Info") {
            tagList(
                h4("Για αυτήν την εργασία"),
                p("Αυτή η διαδικτυακή εφαρμογή έχει σχεδιαστεί για εξόρυξη και ανάλυση δεδομένων."),
                p("Επιτρέπει στους χρήστες να φορτώνουν δεδομένα σε πίνακα, να εκτελούν δισδιάστατες απεικονίσεις, να διεξάγουν διερευνητική ανάλυση δεδομένων και να συγκρίνουν αλγόριθμους ταξινόμησης και ομαδοποίησης."),
                h4("Η Ομάδα μας"),
                p("Η ομάδα μας The Edgers"),
                p("Μέλη Ομάδας:"),
                p("- Γρηγόριος Μαρούλης"),
                p("- Γιώργος Βέλλας"),
                p("- Πέτρος Περαντωνάκης"),
                h4("Εργασίες που εκτελούνται"),
                p("- 2D Visualisation, Docker setup, Github version control, Machine learning algorithms comparison, Software release life cycle, Data handling, Latex report, Uml diagram")
            )
        }
    })

    output$viz_plot <- renderPlotly({
        req(data())
        df <- data()
        df_numeric <- df[sapply(df, is.numeric)]
        if (input$viz_algo == "PCA") {
            pca <- prcomp(df_numeric, center = TRUE, scale. = TRUE)
            pca_data <- data.frame(pca$x, label = df[[ncol(df)]])
            p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(label))) + geom_point()
            ggplotly(p)
        } else if (input$viz_algo == "t-SNE") {
            tsne <- Rtsne(df_numeric, dims = 2)
            tsne_data <- data.frame(tsne$Y, label = df[[ncol(df)]])
            colnames(tsne_data) <- c("Dim1", "Dim2", "label")
            p <- ggplot(tsne_data, aes(x = Dim1, y = Dim2, color = as.factor(label))) + geom_point()
            ggplotly(p)
        }
    })

    output$eda_plots <- renderUI({
        if (input$eda_type == "Histograms") {
            lapply(names(data())[1:(ncol(data()) - 1)], function(column) {
                div(class = "plot-container", plotlyOutput(paste0("hist_", column)))
            })
        } else if (input$eda_type == "Boxplots") {
            lapply(names(data())[1:(ncol(data()) - 1)], function(column) {
                div(class = "plot-container", plotlyOutput(paste0("box_", column)))
            })
        }
    })

    observe({
        req(data())
        df <- data()
        lapply(names(df)[1:(ncol(df) - 1)], function(column) {
            output[[paste0("hist_", column)]] <- renderPlotly({
                p <- ggplot(df, aes_string(x = column)) + geom_histogram(aes(y = ..density..), fill = "blue", alpha = 0.5) + geom_density(color = "red")
                ggplotly(p)
            })

            output[[paste0("box_", column)]] <- renderPlotly({
                p <- ggplot(df, aes_string(y = column)) + geom_boxplot(fill = "blue", alpha = 0.5)
                ggplotly(p)
            })
        })
    })
}

shinyApp(ui = ui, server = server)