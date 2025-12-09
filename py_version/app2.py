import io
import os
from datetime import date, datetime, timedelta
from pathlib import Path
from typing import Dict, List, Literal, Tuple

import faicons
import matplotlib.colors as mpl_colors
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import requests
import shinyswatch
from dotenv import load_dotenv
from ipyleaflet import Map, Marker, SearchControl
from langchain.agents import create_agent

### LangGraph dependencies
from langchain_chroma import Chroma
from langchain_core.messages import AIMessage, HumanMessage, SystemMessage
from langchain_core.tools import tool
from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_tavily import TavilySearch

# from langgraph.checkpoint.postgres import PostgresSaver
# from langgraph.checkpoint.postgres.aio import AsyncPostgresSaver
# from psycopg_pool import ConnectionPool
from langgraph.checkpoint.memory import MemorySaver
from langgraph_swarm import create_handoff_tool, create_swarm
from shiny import (
    App,
    Inputs,
    Outputs,
    Session,
    module,
    reactive,
    render,
    req,
    run_app,
    ui,
)
from shiny.types import ImgData
from shiny_calendar import render_shiny_calendar, shiny_calendar
from shinywidgets import output_widget, render_widget

from tools.utils import (
    calc_bu,
    calc_fu,
    check_bu_cutoff,
    check_fu_cutoff,
    fungicide_recommendation,
)

# load_dotenv()


def _load_secret(env_var: str, fallback_filename: str) -> str:
    """
    Load a secret from an env var or a fallback file.
    - If the env var points to a file path, read its content.
    - If the env var already contains the key, return it.
    - Otherwise, fall back to a repo-local file.
    """
    raw = os.getenv(env_var)
    if raw:
        path = Path(raw)
        if path.exists():
            return path.read_text().strip()
        return raw.strip()

    fallback_path = Path(__file__).parent / fallback_filename
    if fallback_path.exists():
        return fallback_path.read_text().strip()

    raise ValueError(f"{env_var} is not set and {fallback_filename} was not found.")


### Set API's
#
openai_api_key = _load_secret("OPENAI_API_KEY", "OPENAI_API_KEY.txt")
tavily_api_key = _load_secret("TAVILY_API_KEY", "TAVILY_API_KEY.txt")
weather_api_key = _load_secret("WEATHER_API_KEY", "WEATHER_API_KEY.txt")

os.environ["OPENAI_API_KEY"] = openai_api_key
os.environ["TAVILY_API_KEY"] = tavily_api_key
os.environ["WEATHER_API_KEY"] = weather_api_key
#
model = ChatOpenAI(model="gpt-4o", streaming=True, api_key=openai_api_key)
### Construct as an example a Chroma Vector Store


persistent_chroma_db_path = Path(__name__).parent.resolve() / "data" / "chromadb"
vectorstore = Chroma(
    persist_directory=str(persistent_chroma_db_path),
    embedding_function=OpenAIEmbeddings(),
)
retriever = vectorstore.as_retriever()

### Tools

web_tool = TavilySearch(max_results=5)


@tool("retrieve_blog_posts")
def retrieve_blog_posts(query: str) -> str:
    """Search and return information about Lilian Weng blog posts on LLM agents, prompt engineering, and adversarial attacks on LLMs."""
    # BaseRetriever implements Runnable; use invoke to fetch relevant docs
    docs = retriever.invoke(query)
    return "\n\n".join(doc.page_content for doc in docs)


rag_agent = create_agent(
    model,
    [retrieve_blog_posts, create_handoff_tool(agent_name="web_agent")],
    system_prompt="You are rag_agent, a retrieval expert.",
    name="rag_agent",
)

web_agent = create_agent(
    model,
    [
        web_tool,
        create_handoff_tool(
            agent_name="rag_agent",
            description="Transfer to rag_agent, he can help with retrieval.",
        ),
    ],
    system_prompt="You are web_agent, you can search on the web when is needed.",
    name="web_agent",
)

workflow = create_swarm([rag_agent, web_agent], default_active_agent="rag_agent")

checkpointer = MemorySaver()
config = {"configurable": {"thread_id": "1"}}
langgraph_app = workflow.compile(checkpointer=checkpointer)

####

URL_base = "http://api.weatherapi.com/v1"
API_Method_history = "/history.json"
API_Method_forecast = "/forecast.json"
API_key = f"?key={weather_api_key}"

www_dir = Path(__file__).parent.resolve() / "www"
css_file = Path(__file__).parent / "www" / "css" / "styles.css"
css_app2_file = Path(__file__).parent / "www" / "css" / "app2.css"
js_app2_file = Path(__file__).parent / "www" / "js" / "app2.js"

df_vars_res = pd.read_csv(
    Path(__name__).parent.resolve() / "data" / "potato-res-data.csv", sep=";"
)

countries: List[str] = df_vars_res["Country"].unique().tolist()

###


app_ui = ui.page_fluid(
    ui.head_content(
        ui.include_css(css_file),
        ui.include_css(css_app2_file),
        ui.include_js(js_app2_file),
    ),
    ui.layout_sidebar(
        ui.sidebar(
            ui.h4("Field setup"),
            ui.div(
                {"class": "sidebar-section"},
                ui.h6("Dates"),
                ui.input_date("planting_date", label="Planting Date"),
                ui.input_date("emergence_date", label="Emergence Date"),
                ui.input_date("forecast_date", label="Forecast Date"),
            ),
            ui.div(
                {"class": "sidebar-section"},
                ui.h6("Location"),
                ui.input_selectize("country", label="Country", choices=countries),
            ),
            ui.div(
                {"class": "sidebar-section"},
                ui.h6("Crop"),
                ui.output_ui("varieties_ui"),
            ),
            ui.div(
                {"class": "sidebar-section sidebar-footer"},
                ui.a(
                    faicons.icon_svg("github"),
                    " Source",
                    href="https://github.com/CENTRO-INTERNACIONAL-DE-LA-PAPA/Lateblight",
                    target="_blank",
                    class_="sidebar-link",
                ),
            ),
            title="Dashboard",
            bg="#eefbf0",
        ),
        ui.div(
            {"class": "main-shell"},
            ui.card(
                {"class": "hero-card"},
                ui.layout_columns(
                    ui.div(
                        {"class": "hero-text"},
                        ui.div(
                            {"class": "hero-badge"},
                            faicons.icon_svg("seedling"),
                            ui.span("Early access"),
                        ),
                        ui.h2("Lateblight Advisor"),
                        ui.p(
                            "Monitor risk, plan fungicide applications, and track your season in one place."
                        ),
                    ),
                    ui.div(
                        {"class": "hero-actions"},
                        ui.input_action_button(
                            "run_model",
                            label="Run model",
                            icon=faicons.icon_svg("cloud"),
                            class_="btn-accent",
                        ),
                        ui.download_button(
                            "download_data",
                            label="Download result",
                            icon=faicons.icon_svg("download"),
                            class_="btn-ghost",
                        ),
                        ui.input_dark_mode(mode="light", id="dark_mode"),
                    ),
                    col_widths=(8, 4),
                ),
            ),
            ui.output_ui("summary_metrics"),
            ui.layout_columns(
                ui.card(
                    {"class": "map-card"},
                    ui.card_header("Field location"),
                    ui.div(
                        {"class": "pill-row copy-coords"},
                        ui.span(faicons.icon_svg("location-dot"), class_="pill"),
                        ui.span(
                            ui.output_ui("last_click_text_pill"),
                            class_="pill pill-muted",
                        ),
                        ui.input_action_button(
                            "locate_me",
                            label="Use my location",
                            icon=faicons.icon_svg("location-crosshairs"),
                            class_="pill-btn",
                        ),
                        ui.span("Click to copy", class_="overlay-hint"),
                    ),
                    output_widget("map"),
                    full_screen=True,
                ),
                ui.layout_columns(
                    ui.card(
                        ui.card_header("Assistant"),
                        ui.p(
                            'Try: "Summarize weather risks for the next week" or "What’s the next spray?"',
                            class_="assistant-hint",
                        ),
                        ui.chat_ui("chat"),
                        full_screen=True,
                    ),
                    ui.card(
                        ui.card_header("Weather trends"),
                        ui.div(
                            {"class": "weather-img"},
                            ui.output_image("weather_trend_img"),
                        ),
                        full_screen=True,
                    ),
                    col_widths=(12),
                ),
                col_widths=(7, 5),
            ),
            ui.layout_columns(
                ui.card(
                    ui.card_header(
                        ui.div(
                            {"class": "card-header-row"},
                            ui.span("Season calendar"),
                            ui.span("Fungicide event", class_="legend-dot"),
                        )
                    ),
                    shiny_calendar("calendar"),
                    full_screen=True,
                ),
                ui.card(
                    ui.card_header("Fungicide plan"),
                    ui.output_data_frame("recommendations_table"),
                    full_screen=True,
                ),
                col_widths=(8, 4),
            ),
        ),
        bg="#f8fbfa",
        border=False,
    ),
    ui.card(
        ui.output_image("cip_logo"),
        ui.markdown(
            "CIP thanks all donors and organizations that globally support its work through their contributions to the CGIAR Trust Fund."
        ),
        ui.markdown(
            "This publication is copyrighted by the International Potato Center (CIP). It is licensed for use under the Creative Commons Attribution 4.0 International License."
        ),
        ui.card_footer(
            faicons.icon_svg("creative-commons"), faicons.icon_svg("person")
        ),
        fill=False,
        id="footer_cip_logo",
    ),
    window_title="Lateblight App",
    theme=shinyswatch.theme.litera,
)


def server(input, output, session: Session):
    last_click = reactive.Value(None)
    model_result = reactive.Value(None)
    recommendations_val = reactive.Value(None)
    weather_daily = reactive.Value(None)
    hrlimit = 85
    map_marker = reactive.Value(None)
    map_widget = reactive.Value(None)

    @render.ui
    def varieties_ui():
        opts = (
            df_vars_res[df_vars_res["Country"] == input.country()]["Cultivar"]
            .unique()
            .tolist()
        )
        return ui.input_selectize(
            "potato_variety", label="Potato Variety", choices=opts, selected=opts[0]
        )

    @render_widget
    def map():
        m = Map(center=(-11.5, -75.73), zoom=8)
        marker = Marker(location=(-11.5, -75.73))
        m.add(marker)
        map_marker.set(marker)
        map_widget.set(m)

        # Geolocation control (import path varies by ipyleaflet version)
        try:
            from ipyleaflet import LocateControl  # type: ignore
        except ImportError:
            try:
                from ipyleaflet.leaflet import LocateControl  # type: ignore
            except ImportError:
                LocateControl = None  # type: ignore

        if LocateControl is not None:
            locate = LocateControl(position="topleft", flyTo=True)
            m.add(locate)

            def on_location_found(**kwargs):
                loc = kwargs.get("latlng") or kwargs.get("location")
                if loc:
                    lat, lon = loc
                    marker.location = (lat, lon)
                    last_click.set((lat, lon))

            locate.on_location_found(on_location_found)

        def handle_click(**kwargs):
            if kwargs.get("type") == "click":
                coords = kwargs.get("coordinates")
                print(f"Clicked at {coords}")
                last_click.set(coords)
                marker.location = coords

        m.scroll_wheel_zoom = True
        search = SearchControl(
            position="topleft",
            url="https://nominatim.openstreetmap.org/search?format=json&q={s}",
            marker=marker,
        )

        def on_search_found(**kwargs):
            coords = kwargs.get("location")
            if coords:
                print(f"Search found location: {coords}")
                marker.location = coords
                last_click.set(coords)

        search.on_location_found(on_search_found)
        m.add(search)
        m.on_interaction(handle_click)

        return m

    @reactive.effect
    def sync_geolocate():
        loc = input.geolocate()
        marker = map_marker.get()
        m = map_widget.get()
        if loc is None or marker is None or m is None:
            return
        lat = loc.get("lat")
        lon = loc.get("lon")
        if lat is None or lon is None:
            return
        coords = (lat, lon)
        marker.location = coords
        m.center = coords
        last_click.set(coords)

    def _format_coords():
        coords = last_click.get()
        if coords is None:
            return None, "Select a location on the map"
        lat, lon = coords
        return (lat, lon), f"{lat:.2f}, {lon:.2f}"

    @render.text
    def last_click_text_pill():
        _, label = _format_coords()
        return label

    @render.text
    def last_click_text_box():
        coords, label = _format_coords()
        if coords is None:
            return label
        lat, lon = coords
        return f"Latitude: {lat:.2f}\nLongitude: {lon:.2f}"

    @render.ui
    def summary_metrics():
        df = model_result.get()
        rec_df = recommendations_val.get()
        coords = last_click.get()

        def fmt_date(val):
            if isinstance(val, (datetime, date)):
                return val.strftime("%b %d, %Y")
            return str(val)

        def metric(title, value, subtitle, icon):
            return ui.div(
                {"class": "metric-card"},
                ui.div({"class": "metric-icon"}, faicons.icon_svg(icon)),
                ui.div(
                    {"class": "metric-body"},
                    ui.p(title, class_="metric-title"),
                    ui.h4(value, class_="metric-value"),
                    ui.p(subtitle, class_="metric-subtitle"),
                ),
            )

        next_app = "Pending"
        subtitle_app = "Run model to generate schedule"
        rec_count = 0 if rec_df is None else len(rec_df)
        days_modelled = 0 if df is None else len(df)

        if df is not None and not df.empty:
            app_rows = df[(df["APP"] > 0) & pd.notna(df["APP_TRIGGER"])]
            if not app_rows.empty:
                app_rows = app_rows.sort_values("date")
                next_app = fmt_date(app_rows.iloc[0]["date"])
                subtitle_app = f"{len(app_rows)} recommended in window"
            else:
                next_app = "No triggers yet"
                subtitle_app = "Model run complete"

        coord_text = (
            f"{coords[0]:.2f}, {coords[1]:.2f}" if coords else "Select a point on map"
        )

        return ui.div(
            {"class": "summary-row"},
            ui.layout_columns(
                metric("Next application", next_app, subtitle_app, "calendar-check"),
                metric(
                    "Planned sprays",
                    str(rec_count),
                    "Total recommendations",
                    "flask",
                ),
                metric(
                    "Location",
                    coord_text,
                    "Click on the map to update",
                    "location-dot",
                ),
                col_widths=(4, 4, 4),
            ),
        )

    @render.image(delete_file=True)
    def weather_trend_img():
        df = weather_daily.get()
        if df is None or df.empty or "date" not in df.columns:
            return None

        plot_df = df.copy()
        plot_df["date"] = pd.to_datetime(plot_df["date"])
        plot_df = plot_df.sort_values("date")

        for col in ("temp_c", "humidity", "precip_mm"):
            if col not in plot_df.columns:
                return None

        dates = plot_df["date"]
        temps = plot_df["temp_c"]
        hums = plot_df["humidity"]
        rains = plot_df["precip_mm"]

        planting = input.planting_date()
        forecast = input.forecast_date()
        today = date.today()

        dmin = dates.min()
        dmax = dates.max()

        span1_start = max(dmin, pd.to_datetime(planting))
        span1_end = min(dmax, pd.to_datetime(min(today, forecast)))
        span2_start = pd.to_datetime(max(today, span1_end.date()))
        span2_end = min(dmax, pd.to_datetime(forecast))

        fig, ax1 = plt.subplots(figsize=(6.8, 3.4))
        ax2 = ax1.twinx()

        ax1.plot(dates, temps, label="Temp (°C)", color="#ff7f0e", linewidth=2)
        ax1.plot(
            dates,
            hums,
            label="Humidity (%)",
            color="#1f77b4",
            linestyle="--",
            linewidth=1.8,
        )
        ax2.bar(
            dates, rains, width=0.8, alpha=0.22, color="#2ca02c", label="Precip (mm)"
        )

        ax1.set_ylabel("Temp (°C) / Humidity (%)")
        ax2.set_ylabel("Precip (mm)")
        ax1.set_xlabel("")

        ax1.xaxis.set_major_formatter(mdates.DateFormatter("%b %d"))
        ax1.tick_params(axis="x", labelsize=8)
        fig.autofmt_xdate()

        # Shading
        try:
            ax1.axvspan(
                span1_start,
                span1_end,
                color="#dbeafe",
                alpha=0.6,
                label="Observed → today",
            )
        except Exception:
            pass
        try:
            ax1.axvspan(
                span2_start,
                span2_end,
                color="#fff4e5",
                alpha=0.8,
                label="Today → forecast",
            )
        except Exception:
            pass

        # Legends
        lines1, labels1 = ax1.get_legend_handles_labels()
        lines2, labels2 = ax2.get_legend_handles_labels()
        y_max = max(float(temps.max()), float(hums.max()))
        y_upper = max(40, min(150, y_max * 1.15))
        ax1.set_ylim(bottom=0, top=y_upper)
        # Fungicide applications (if available)
        app_df = model_result.get()
        app_handles: List = []
        app_labels: List = []
        if app_df is not None and not app_df.empty and "APP" in app_df.columns:
            app_events = app_df[
                (app_df["APP"] > 0) & pd.notna(app_df.get("APP_TRIGGER"))
            ]
            if not app_events.empty and "date" in app_events.columns:
                app_dates = pd.to_datetime(app_events["date"])
                y_scatter = y_upper * 0.9
                scatter = ax1.scatter(
                    app_dates,
                    [y_scatter] * len(app_dates),
                    color="#d62728",
                    marker="o",
                    zorder=5,
                    label="Fungicide app",
                )
                app_handles.append(scatter)
                app_labels.append("Fungicide app")

        # Humidity limit line
        hline = ax1.axhline(
            hrlimit,
            color="#d62728",
            linestyle="--",
            linewidth=1.2,
            alpha=0.6,
            label=f"HR limit ({hrlimit}%)",
        )
        app_handles.append(hline)
        app_labels.append(f"HR limit ({hrlimit}%)")

        ax1.legend(
            lines1 + lines2 + app_handles,
            labels1 + labels2 + app_labels,
            loc="upper center",
            bbox_to_anchor=(0.5, -0.2),
            ncol=2,
            frameon=False,
        )

        ax1.grid(True, linestyle="--", alpha=0.15)
        fig.tight_layout()
        buf = io.BytesIO()
        fig.savefig(buf, format="png", dpi=150, bbox_inches="tight")
        plt.close(fig)
        # Write to a temp file so render.image can serve it
        import tempfile

        tmp = tempfile.NamedTemporaryFile(delete=False, suffix=".png")
        tmp.write(buf.getvalue())
        tmp.flush()
        tmp.close()
        return {"src": tmp.name, "content_type": "image/png"}

    def fetch_weather_data():
        """
        Fetches weather data from the API for a given date range and location.
        day0 and dayn should be Python date objects.
        Returns a pandas DataFrame with daily summaries.
        """

        print("Run Model button clicked!")
        day0 = input.planting_date()  # or input.emergence_date() as needed
        dayn = input.forecast_date()

        coords = last_click.get()
        if coords is None:
            print("No location selected. Please click on the map to choose a location.")
            return

        lat, lon = coords
        print(f"Fetching weather data from {day0} to {dayn} at {lat}, {lon}")

        API_q_coordinates = f"&q={lat},{lon}"
        sysdate = date.today()
        dayinitial = day0
        dayfinal = dayn
        difftime_day = (dayfinal - dayinitial).days

        def split_date_range(start_date, end_date, max_days=30):
            """Splits a date range into non-overlapping chunks of at most max_days."""
            chunks = []
            current_start = start_date
            while current_start <= end_date:
                current_end = min(
                    current_start + timedelta(days=max_days - 1), end_date
                )
                chunks.append((current_start, current_end))
                current_start = current_end + timedelta(days=1)
            return chunks

        def process_json_hourly(data_json):
            forecast_days = data_json.get("forecast", {}).get("forecastday", [])
            hours = []
            for fd in forecast_days:
                # Each forecast day should have an 'hour' list
                hours.extend(fd.get("hour", []))
            if not hours:
                return pd.DataFrame()
            df = pd.DataFrame(hours)
            df = df[["time", "temp_c", "humidity", "precip_mm"]].copy()
            df.columns = ["date_hour", "tmean", "hr", "pp"]
            df["date_hour"] = pd.to_datetime(df["date_hour"], format="%Y-%m-%d %H:%M")
            df["tmean"] = pd.to_numeric(df["tmean"], errors="coerce")
            df["hr"] = pd.to_numeric(df["hr"], errors="coerce")
            df["pp"] = pd.to_numeric(df["pp"], errors="coerce")
            # Compute the hour and "period" indicator (hours 13 and after belong to the same day;
            # before 13 are assigned to the previous day)
            df["hour"] = df["date_hour"].dt.hour
            df["period"] = df["date_hour"].apply(
                lambda dt: dt.date()
                if dt.hour >= 13
                else (dt - timedelta(days=1)).date()
            )
            return df

        def aggregate_df(df):
            """Group the hourly data by period and compute summary statistics."""
            if df.empty:
                return pd.DataFrame()

            def agg_func(g):
                count_hr = (g["hr"] >= hrlimit).sum()
                tavg = (
                    g.loc[g["hr"] >= hrlimit, "tmean"].mean()
                    if count_hr > 0
                    else np.nan
                )
                return pd.Series(
                    {
                        "hr90_hour": count_hr,
                        "tavg_C": tavg,
                        "rain_mm": g["pp"].sum(),
                        "avg_hr": g["hr"].mean(),
                    }
                )

            grouped = (
                df.groupby("period")
                .apply(agg_func)
                .reset_index()
                .rename(columns={"period": "date"})
            )
            return grouped

        def aggregate_daily_means(df):
            """Simple daily means/sums straight from API hourly data."""
            if df.empty:
                return pd.DataFrame()
            return (
                df.groupby("period")
                .agg(
                    temp_c=("tmean", "mean"),
                    humidity=("hr", "mean"),
                    precip_mm=("pp", "sum"),
                )
                .reset_index()
                .rename(columns={"period": "date"})
            )

        # ---------- Branch 1: Historical only (dayinitial and dayfinal both before today) ----------
        if dayinitial < sysdate and dayfinal < sysdate:
            if difftime_day <= 30:
                API_date = f"&dt={dayinitial}&end_dt={dayfinal}"
                PreDataAPI = (
                    URL_base
                    + API_Method_history
                    + API_key
                    + API_q_coordinates
                    + API_date
                )
                dataAPI_json = requests.get(PreDataAPI).json()
                df = process_json_hourly(dataAPI_json)
                weather_daily.set(aggregate_daily_means(df))
                daily_summary = aggregate_df(df)
                return daily_summary

            elif difftime_day > 30:
                # Split into non-overlapping chunks of 30 days
                chunks = split_date_range(dayinitial, dayfinal, max_days=30)
                list_dfs = []
                for d0, d1 in chunks:
                    API_date = f"&dt={d0}&end_dt={d1}"
                    PreDataAPI = (
                        URL_base
                        + API_Method_history
                        + API_key
                        + API_q_coordinates
                        + API_date
                    )
                    dataAPI_json = requests.get(PreDataAPI).json()
                    df_chunk = process_json_hourly(dataAPI_json)
                    list_dfs.append(df_chunk)
                df_all = pd.concat(list_dfs).reset_index(drop=True)
                weather_daily.set(aggregate_daily_means(df_all))
                input_runsimcast = aggregate_df(df_all)
                print(input_runsimcast)
                return input_runsimcast

        # ---------- Branch 2: Mixed historical and forecast (dayinitial before today, dayfinal today or later) ----------
        elif dayinitial < sysdate and dayfinal >= sysdate:
            # Historical portion: from dayinitial to yesterday
            dayinitial_h = dayinitial
            dayfinal_h = sysdate - timedelta(days=1)
            difftime_day_h = (dayfinal_h - dayinitial_h).days

            if difftime_day_h <= 30:
                API_date = f"&dt={dayinitial_h}&end_dt={dayfinal_h}"
                PreDataAPI = (
                    URL_base
                    + API_Method_history
                    + API_key
                    + API_q_coordinates
                    + API_date
                )
                dataAPI_json = requests.get(PreDataAPI).json()
                df_hist = process_json_hourly(dataAPI_json)
                daily_summary_h = aggregate_df(df_hist)
                df_hist_all = df_hist

            elif difftime_day_h > 30:
                chunks = split_date_range(dayinitial_h, dayfinal_h, max_days=30)
                list_dfs = []
                for d0, d1 in chunks:
                    API_date = f"&dt={d0}&end_dt={d1}"
                    PreDataAPI = (
                        URL_base
                        + API_Method_history
                        + API_key
                        + API_q_coordinates
                        + API_date
                    )
                    dataAPI_json = requests.get(PreDataAPI).json()
                    df_chunk = process_json_hourly(dataAPI_json)
                    list_dfs.append(df_chunk)
                df_hist_all = pd.concat(list_dfs).reset_index(drop=True)
                daily_summary_h = aggregate_df(df_hist_all)
            # Forecast portion: from today to dayfinal
            dayinitial_f = sysdate
            dayfinal_f = dayfinal
            difftime_day_f = (dayfinal_f - dayinitial_f).days
            # Here we assume the forecast API uses a parameter &days=
            API_date = f"&days={(difftime_day_f + 1)}"
            PreDataAPI = (
                URL_base + API_Method_forecast + API_key + API_q_coordinates + API_date
            )
            dataAPI_json = requests.get(PreDataAPI).json()
            df_forecast = process_json_hourly(dataAPI_json)
            daily_summary_f = aggregate_df(df_forecast)
            df_forecast_all = df_forecast
            # Combine the two parts and group by date if needed
            input_runsimcast_h_y_f = pd.concat(
                [daily_summary_h, daily_summary_f], ignore_index=True
            )
            final_summary = (
                input_runsimcast_h_y_f.groupby("date")
                .agg(
                    {
                        "hr90_hour": "sum",
                        "tavg_C": "mean",
                        "rain_mm": "sum",
                        "avg_hr": "mean",
                    }
                )
                .reset_index()
            )
            weather_daily.set(
                aggregate_daily_means(
                    pd.concat([df_hist_all, df_forecast_all]).reset_index(drop=True)
                )
            )
            return final_summary

        # ---------- Branch 3: Forecast only (both dayinitial and dayfinal on or after today) ----------
        elif dayinitial >= sysdate and dayfinal >= sysdate:
            API_date = f"&days={(difftime_day + 1)}"
            PreDataAPI = (
                URL_base + API_Method_forecast + API_key + API_q_coordinates + API_date
            )
            dataAPI_json = requests.get(PreDataAPI).json()
            df = process_json_hourly(dataAPI_json)
            weather_daily.set(aggregate_daily_means(df))
            daily_summary = aggregate_df(df)
            return daily_summary

        # If none of the above conditions match, return an empty DataFrame.
        return pd.DataFrame()

    def simcast_model(df_in_simcast, vt, date_emergence, date0):
        """
        Simulate the model over the daily summary DataFrame.

        Parameters:
            df_in_simcast: pd.DataFrame
                Daily summary with columns: "date", "hr90_hour", "tavg_C", "rain_mm".
            vt: str or list-like
                The variety type. If list-like, the first element is used.
            date_emergence: date/datetime
                The date of emergence.
            date0: date/datetime
                The reference date (e.g., planting date).

        Returns:
            pd.DataFrame: Original dataframe with additional simulation columns:
                - BU: Daily blight units.
                - FU: Daily fungicide units.
                - BUA: Accumulated blight units.
                - FUA: Accumulated fungicide units.
                - ABU: Flag for application based on BU cutoff (1 if applied).
                - AFU: Flag for application based on FU cutoff (1 if applied).
                - APP: Cumulative count of applications.
                - APP_TRIGGER: Trigger type ("BU", "FU", "Both", "Forced", or None).
        """
        # Fill missing average temperature values with the mean
        df = df_in_simcast.copy()
        df["tavg_C"] = df["tavg_C"].fillna(df["tavg_C"].mean())

        # Ensure vt is a string (if list-like, take the first element)
        if isinstance(vt, (list, tuple)):
            vt = str(vt[0])
        else:
            vt = str(vt)

        # Initialization of simulation variables
        firstApplication = True
        app_ctr = 0  # Total number of applications made
        days_since_app = 0  # Days since last application
        min_day = 7

        bua = 0  # Accumulated Blight Units
        fua = 0  # Accumulated Fungicide Units

        n = len(df)

        # Lists to record daily values:
        bu_vec = [0] * n  # daily BU
        fu_vec = [0] * n  # daily FU
        bua_vec = [0] * n  # running accumulated BU
        fua_vec = [0] * n  # running accumulated FU
        abu_vec = [0] * n  # flag triggered by BU cutoff (1 if applied)
        afu_vec = [0] * n  # flag triggered by FU cutoff (1 if applied)
        app_vec = [0] * n  # cumulative count of applications
        trigger_vec = [None] * n  # record trigger information

        # Compute forced application day as difference in days between emergence and planting
        forced_app_day = (date_emergence - date0).days

        # Iterate over each day (row) in the dataframe
        for k in range(n):
            app_trigger = None
            # Extract daily values (assumes the columns exist in df)
            day = df.iloc[k]["date"]
            hhr = float(df.iloc[k]["hr90_hour"])
            htavg = float(df.iloc[k]["tavg_C"])
            rain = float(df.iloc[k]["rain_mm"])

            # Calculate today's blight units using the provided function
            bu = calc_bu(hhr, htavg, vt)
            bu_vec[k] = bu

            # Update days since last application and calculate fungicide units
            days_since_app += 1
            fu = calc_fu(rain, days_since_app)
            fua += fu

            # Accumulate blight units (bu is added, even if zero)
            bua += bu

            # Store accumulators for today
            bua_vec[k] = bua
            fua_vec[k] = fua
            fu_vec[k] = fu  # daily fungicide units

            # Initialize application flags for today
            abu = 0
            afu = 0

            # Check if an application should be made (only if at least one BU has accumulated)
            if bua > 0:
                cutoff_bu_triggered = check_bu_cutoff(bua, vt)
                cutoff_fu_triggered = check_fu_cutoff(fua, vt)
                if (
                    (cutoff_bu_triggered or cutoff_fu_triggered)
                    and (days_since_app > min_day)
                    and (not firstApplication)
                ):
                    # Determine which cutoff triggered the application
                    if cutoff_bu_triggered and not cutoff_fu_triggered:
                        app_trigger = "BU"
                    elif cutoff_fu_triggered and not cutoff_bu_triggered:
                        app_trigger = "FU"
                    elif cutoff_bu_triggered and cutoff_fu_triggered:
                        app_trigger = "Both"

                    # Record an application
                    abu = 1
                    afu = 1
                    app_ctr += 1
                    days_since_app = 0  # reset day counter
                    # Reset accumulators after an application
                    bua = 0
                    fua = 0

            # Force an application on the forced application day
            if k == forced_app_day:
                abu = 1
                afu = 1
                app_ctr += 1
                days_since_app = 0
                firstApplication = False
                bua = 0
                fua = 0
                app_trigger = "Forced"

            trigger_vec[k] = app_trigger

            # Record the application flags and cumulative count for today
            bu_vec[k] = bu
            fu_vec[k] = fu
            bua_vec[k] = bua
            fua_vec[k] = fua
            abu_vec[k] = abu
            afu_vec[k] = afu
            app_vec[k] = app_ctr

        # Create a DataFrame with the simulation outputs
        output_simcast = pd.DataFrame(
            {
                "BU": bu_vec,
                "FU": fu_vec,
                "BUA": bua_vec,
                "FUA": fua_vec,
                "ABU": abu_vec,
                "AFU": afu_vec,
                "APP": app_vec,
                "APP_TRIGGER": trigger_vec,
            }
        )

        # Combine the original daily summary with the simulation outputs
        output_scmodel = pd.concat([df.reset_index(drop=True), output_simcast], axis=1)
        print("Running simcast model ... Loading")
        return output_scmodel

    @reactive.effect
    @reactive.event(input.run_model)
    def calc_fecth_and_simcast():
        req(last_click.get())
        # Display a progress bar during the model run
        with ui.Progress(min=0, max=100) as p:
            p.set(10, message="Fetching weather data ...")
            df = fetch_weather_data()
            p.set(50, message="Running Simcast model ...")
            if df.empty:
                print("No weather data available.")
                return
            potato_variety = input.potato_variety()
            vt = df_vars_res[df_vars_res["Cultivar"] == potato_variety][
                "Resistance"
            ].values[0]
            date_emergence = input.emergence_date()
            date0 = input.planting_date()
            DSS = simcast_model(df, vt, date_emergence, date0)
            p.set(80, message="Model run completed.")
            model_result.set(DSS)
            recommendations_df = fungicide_recommendation(DSS, vt)
            recommendations_val.set(recommendations_df)
            p.set(100, message="Recommendations ready.")
            print(DSS)
            print(recommendations_df)
            return DSS

    @render.download(filename=lambda: f"{input.potato_variety()}_model_result.csv")
    async def download_data():
        df = model_result.get()
        if df is None:
            yield b""
            return
        # Write CSV content to a StringIO buffer
        buffer = io.StringIO()
        df.to_csv(buffer, index=False)
        # Yield the CSV content as bytes (encoded in UTF-8)
        yield buffer.getvalue().encode("utf-8")

    @render_shiny_calendar
    def calendar():
        df = model_result.get()
        print("Calendar function called. model_result =", df)
        events = []
        if df is not None and not df.empty:
            for idx, row in df.iterrows():
                if (row["APP"] > 0) and pd.notna(row["APP_TRIGGER"]):
                    print(row)
                    # Convert the date to a string if it's a date/datetime object
                    date_val = row["date"]
                    if isinstance(date_val, (datetime, date)):
                        date_str = date_val.isoformat()
                    else:
                        date_str = str(date_val)
                    events.append(
                        {
                            "id": f"app_{idx}",
                            "title": f"{row['APP_TRIGGER']}",
                            "start": date_str,  # using the same date as start and end
                            "end": date_str,
                            "backgroundColor": "red",
                            "borderColor": "red",
                        }
                    )
            # Use the earliest event date as the calendar's initial date
            try:
                initial_date = min(event["start"] for event in events)
            except ValueError:
                initial_date = datetime.today().strftime("%Y-%m-%d")
        else:
            initial_date = datetime.today().strftime("%Y-%m-%d")

        # Return a calendar configuration dictionary inside a list
        return [
            {
                "eventClick": "",  # JS callback string if needed
                "initialDate": initial_date,
                "selectable": True,
                "initialView": "dayGridMonth",
                "events": events,
            }
        ]

    @render.data_frame
    def recommendations_table():
        df = recommendations_val.get()
        if df is None:
            return pd.DataFrame()
        return render.DataTable(df, selection_mode="rows")

    @render.ui
    def rows():
        rows = recommendations_table.cell_selection()["rows"]
        selected = ", ".join(str(i) for i in sorted(rows)) if rows else "None"
        return f"Rows selected: {selected}"

    @render.image
    def cip_logo():
        if input.dark_mode() == "light":
            file = www_dir / "img" / "inverted_cip_cgiar_corrected.png"
        else:
            file = www_dir / "img" / "cip_cgiar.png"

        img: ImgData = {"src": str(file)}
        print(f"Including the following file:{file}")
        return img

    chat = ui.Chat(id="chat")

    @chat.on_user_submit
    async def _():
        user_message = chat.user_input()

        events = langgraph_app.stream(
            {
                "messages": [
                    {
                        "role": "user",
                        "content": user_message,
                    },
                ],
            },
            config,
            stream_mode="values",
        )

        for event in events:
            if "messages" in event and isinstance(event["messages"][-1], AIMessage):
                output_langgraph = event["messages"][-1].content

        await chat.append_message(output_langgraph)


app = App(app_ui, server, static_assets=www_dir)
