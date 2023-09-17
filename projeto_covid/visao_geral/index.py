import dash
from dash.html import Div


dash.register_page(
    __name__, title="Visão Geral", path="/visao-geral", name="Visão Geral", order=0
)

def layout() -> Div:
    """Gera o Layout da aba geral."""
    return Div(
        [
            Div("oi", style={"height": "64px"}),
            Div(
                [
                    'Gráficos legais'
                ],
            ),
        ]
    )
