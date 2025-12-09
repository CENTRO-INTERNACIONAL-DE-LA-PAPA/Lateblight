pipeline {
    agent any

    environment {
        GITHUB_REPO        = 'git@github.com:CENTRO-INTERNACIONAL-DE-LA-PAPA/Lateblight.git'
        GITHUB_CREDENTIALS = 'github-ssh-cip'
        APP_FOLDER         = 'py_version'
        COMPOSE_FILE       = "${APP_FOLDER}/docker-compose.yml"
    }

    stages {
        stage('Checkout') {
            steps {
                git branch: 'main',
                    url: "${GITHUB_REPO}",
                    credentialsId: "${GITHUB_CREDENTIALS}"
            }
        }

        stage('Write secrets') {
            steps {
                withCredentials([
                    string(credentialsId: 'openai-api-key',  variable: 'OPENAI_API_KEY'),
                    string(credentialsId: 'tavily-api-key',  variable: 'TAVILY_API_KEY'),
                    string(credentialsId: 'weather-api-key', variable: 'WEATHER_API_KEY')
                ]) {
                    sh '''
                        set -e
                        cd "${APP_FOLDER}"
                        printf "%s" "$OPENAI_API_KEY"  > OPENAI_API_KEY.txt
                        printf "%s" "$TAVILY_API_KEY"  > TAVILY_API_KEY.txt
                        printf "%s" "$WEATHER_API_KEY" > WEATHER_API_KEY.txt
                        chmod 600 *_API_KEY.txt
                    '''
                }
            }
        }

        stage('Deploy with Compose') {
            steps {
                sh '''
                    set -e
                    cd "${APP_FOLDER}"
                    docker-compose -f docker-compose.yml down || true
                    docker-compose -f docker-compose.yml up -d --build
                '''
            }
        }

        stage('Cleanup secrets from workspace') {
            steps {
                sh '''
                    cd "${APP_FOLDER}" && rm -f OPENAI_API_KEY.txt TAVILY_API_KEY.txt WEATHER_API_KEY.txt
                '''
            }
        }
    }

    post {
        always {
            echo 'Pipeline finished.'
        }
    }
}
