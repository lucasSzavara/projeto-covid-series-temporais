"""O ponto de entrada da aplicação."""
import logging
import os

from app import app

logging.basicConfig(
    level=os.environ.get("logging_level", "WARNING"),
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)

if __name__ == "__main__":
    logging.info("Running")
    app.run_server(debug=True)
