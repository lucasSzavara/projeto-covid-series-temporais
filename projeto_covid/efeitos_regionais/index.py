import dash
from dash.html import Div


dash.register_page(
    __name__, title="Efeitos regionais", path="/efeitos-regionais", name="Efeitos regionais", order=1
)

def layout() -> Div:
    """Gera o Layout da aba geral."""
    return Div(
        [
            Div("oi", style={"height": "64px"}),
            Div(
                [
                    'Gráficos de efeitos regionais'
                ],
            ),
        ]
    )
