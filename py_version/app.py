import io
import os
from datetime import date, datetime, timedelta
from pathlib import Path
from typing import Dict, List, Literal, Tuple

import faicons
import matplotlib.colors as mpl_colors
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

os.environ["OPENAI_API_KEY"] = openai_api_key
os.environ["TAVILY_API_KEY"] = tavily_api_key
#
model = ChatOpenAI(model="gpt-4o", streaming=True, api_key=openai_api_key)
### Construct as an example a Chroma Vector Store


persistent_chroma_db_path = Path(__name__).parent.resolve() / "data" / "chromadb"
vectorstore = Chroma(
    persist_directory=str(persistent_chroma_db_path),
    embedding_function=OpenAIEmbeddings(),
)

### Tools

web_tool = TavilySearch(max_results=5)


@tool("retrieve_blog_posts")
def retrieve_blog_posts(query: str) -> str:
    """Search and return information about Lilian Weng blog posts on LLM agents, prompt engineering, and adversarial attacks on LLMs."""
    # BaseRetriever implements Runnable; use invoke to fetch relevant docs
    docs = vectorstore.similarity_search(query)
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
# weather_api_key_file = os.getenv("WEATHER_API_KEY")

# try:
#     # Ensure the secret file exists
#     with open(weather_api_key_file, 'r') as file:
#         weather_api_key = file.read().strip()
# except FileNotFoundError:
#     raise ValueError(f"API key file not found at {weather_api_key_file}. Ensure the secret is properly configured.")

# os.environ["API_key"] = weather_api_key
# API_key = os.getenv("API_key")
API_key = "?key=28884cc8d61e46cbb3793258221912"

www_dir = Path(__file__).parent.resolve() / "www"
css_file = Path(__file__).parent / "www" / "css" / "styles.css"

df_vars_res = pd.read_csv(
    Path(__name__).parent.resolve() / "data" / "potato-res-data.csv", sep=";"
)

countries: List[str] = df_vars_res["Country"].unique().tolist()

###


app_ui = ui.page_fluid(
    ui.navset_underline(
        ui.nav_panel(
            "Dashboard",
            ui.layout_sidebar(
                ui.sidebar(
                    ui.input_date("planting_date", label="Planting Date"),
                    ui.input_date("emergence_date", label="Emergence Date"),
                    ui.input_date("forecast_date", label="Forecast Date"),
                    ui.input_selectize("country", label="Country", choices=countries),
                    ui.output_ui("varieties_ui"),
                    title="Dashboard",
                ),
                ui.card(
                    ui.layout_columns(
                        ui.card(output_widget("map"), full_screen=True),
                        ui.layout_columns(
                            ui.card(ui.chat_ui("chat"), full_screen=True),
                            ui.card(
                                ui.value_box(
                                    title="Coordinates",
                                    showcase=faicons.icon_svg("location-dot"),
                                    value=ui.output_ui("last_click_text"),
                                    theme="bg-gradient-blue-purple",
                                )
                            ),
                            col_widths=(12),
                        ),
                        col_widths=(8, 4),
                    ),
                ),
                # ui.layout_columns(
                # ui.input_action_button("run_model", label="Run Model", icon=faicons.icon_svg("cloud")),
                # ui.download_button("download_data", label="Download Model Result", icon=faicons.icon_svg("download")),
                # col_widths=(6,6)
                # ),
                ui.layout_columns(
                    ui.card(shiny_calendar("calendar"), full_screen=True),
                    ui.card(
                        ui.card_header("Fungicide Applications"),
                        ui.output_data_frame("recommendations_table"),
                        full_screen=True,
                    ),
                ),
            ),
        ),
        ui.nav_control(
            ui.input_action_button(
                "run_model", label="Run Model", icon=faicons.icon_svg("cloud")
            ),
            ui.download_button(
                "download_data",
                label="Download Model Result",
                icon=faicons.icon_svg("download"),
            ),
        ),
        ui.nav_spacer(),
        ui.nav_control(
            ui.a(
                faicons.icon_svg("github"),
                href="https://github.com/CENTRO-INTERNACIONAL-DE-LA-PAPA/Lateblight",
                target="_blank",
            )
        ),
        ui.nav_control(ui.input_dark_mode(mode="light", id="dark_mode")),
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
    ui.include_css(css_file),
    window_title="Lateblight App",
    theme=shinyswatch.theme.litera,
)


def server(input, output, session: Session):
    last_click = reactive.Value(None)
    model_result = reactive.Value(None)
    recommendations_val = reactive.Value(None)
    hrlimit = 85

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

    @render.text
    def last_click_text():
        if last_click.get() is None:
            txt = "Select a location on the map"
        else:
            txt = last_click.get()
            txt = f"Latitude: {txt[0]:.2f}\nLongitude: {txt[1]:.2f}" if txt else "None"
        return f"{txt}"

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
                    daily_summary = aggregate_df(df_chunk)
                    list_dfs.append(daily_summary)
                input_runsimcast = (
                    pd.concat(list_dfs).drop_duplicates().reset_index(drop=True)
                )
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
                    daily_summary = aggregate_df(df_chunk)
                    list_dfs.append(daily_summary)
                daily_summary_h = (
                    pd.concat(list_dfs).drop_duplicates().reset_index(drop=True)
                )
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
            return final_summary

        # ---------- Branch 3: Forecast only (both dayinitial and dayfinal on or after today) ----------
        elif dayinitial >= sysdate and dayfinal >= sysdate:
            API_date = f"&days={(difftime_day + 1)}"
            PreDataAPI = (
                URL_base + API_Method_forecast + API_key + API_q_coordinates + API_date
            )
            dataAPI_json = requests.get(PreDataAPI).json()
            df = process_json_hourly(dataAPI_json)
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
