import numpy as np

def in_range(val, minimum, maximum):
    """Returns True if val is between minimum and maximum (inclusive)."""
    return minimum <= val <= maximum

def in_range2(val, minimum, maximum):
    """Returns True if val is greater than minimum and less than or equal to maximum."""
    return minimum < val <= maximum

def calc_bu(hhr, htavg, vt):
    """
    Calculate blight units based on hours > 90, average temperature, and variety type.
    vt: "s", "ms", "mr", "r", or "hr"
    """
    if htavg > 27 and hhr == 24:
        return 0
    elif in_range2(htavg, 22.5, 27):
        if vt == "s":
            if hhr == 6:
                return 0
            elif in_range(hhr, 7.0, 9.0):
                return 1
            elif in_range(hhr, 10.0, 12.0):
                return 2
            elif in_range(hhr, 13.0, 15.0):
                return 3
            elif in_range(hhr, 16.0, 18.0):
                return 4
            elif in_range(hhr, 19.0, 24.0):
                return 5
            else:
                return 0
        elif vt == "ms":
            if hhr == 9:
                return 0
            elif in_range(hhr, 10.0, 18.0):
                return 1
            elif in_range(hhr, 19.0, 24.0):
                return 2
            else:
                return 0
        elif vt in ("mr", "r", "hr"):
            if hhr == 15:
                return 0
            elif in_range(hhr, 16.0, 24.0):
                return 1
            else:
                return 0
        else:
            return 0
    elif in_range2(htavg, 12.5, 22.5):
        if vt == "s":
            if hhr == 6:
                return 0
            elif in_range(hhr, 7.0, 9.0):
                return 5
            elif in_range(hhr, 10.0, 12.0):
                return 6
            elif in_range(hhr, 13.0, 24.0):
                return 7
            else:
                return 0
        elif vt == "ms":
            if hhr == 6:
                return 0
            elif hhr == 7:
                return 1
            elif hhr == 8:
                return 2
            elif hhr == 9:
                return 3
            elif hhr == 10:
                return 4
            elif in_range(hhr, 11.0, 12.0):
                return 5
            elif in_range(hhr, 13.0, 24.0):
                return 6
            else:
                return 0
        elif vt in ("mr", "r", "hr"):
            if hhr == 6:
                return 0
            elif hhr == 7:
                return 1
            elif hhr == 8:
                return 2
            elif hhr == 9:
                return 3
            elif in_range(hhr, 10.0, 12.0):
                return 4
            elif in_range(hhr, 13.0, 24.0):
                return 5
            else:
                return 0
        else:
            return 0
    elif in_range2(htavg, 7.5, 12.5):
        if vt == "s":
            if hhr == 6:
                return 0
            elif hhr == 7:
                return 1
            elif in_range(hhr, 8.0, 9.0):
                return 2
            elif hhr == 10:
                return 3
            elif in_range(hhr, 11.0, 12.0):
                return 4
            elif in_range(hhr, 13.0, 15.0):
                return 5
            elif in_range(hhr, 16.0, 24.0):
                return 6
            else:
                return 0
        elif vt == "ms":
            if hhr == 6:
                return 0
            elif in_range(hhr, 7.0, 9.0):
                return 1
            elif in_range(hhr, 10.0, 12.0):
                return 2
            elif in_range(hhr, 13.0, 15.0):
                return 3
            elif in_range(hhr, 16.0, 18.0):
                return 4
            elif in_range(hhr, 19.0, 24.0):
                return 5
            else:
                return 0
        elif vt in ("mr", "r", "hr"):
            if hhr == 9:
                return 0
            elif in_range(hhr, 10.0, 12.0):
                return 1
            elif in_range(hhr, 13.0, 15.0):
                return 2
            elif in_range(hhr, 16.0, 24.0):
                return 3
            else:
                return 0
        else:
            return 0
    elif in_range(htavg, 3, 7.5):
        if vt == "s":
            if hhr == 9:
                return 0
            elif in_range(hhr, 10.0, 12.0):
                return 1
            elif in_range(hhr, 13.0, 15.0):
                return 2
            elif in_range(hhr, 16.0, 18.0):
                return 3
            elif in_range(hhr, 19.0, 24.0):
                return 4
            else:
                return 0
        elif vt == "ms":
            if hhr == 12:
                return 0
            elif in_range(hhr, 13.0, 24.0):
                return 1
            else:
                return 0
        elif vt in ("mr", "r", "hr"):
            if hhr == 18:
                return 0
            elif in_range(hhr, 19.0, 24.0):
                return 1
            else:
                return 0
        else:
            return 0
    elif htavg < 3 and hhr == 24:
        return 0
    else:
        return 0
    
def calc_fu(rain, dsa):
    """
    Calculate fungicide units based on rain (mm) and days since application.
    """
    if 0 < rain < 1:
        return 1
    else:
        if dsa == 1:
            if in_range(rain, 1.0, 1.45):
                return 4
            elif in_range2(rain, 1.45, 3.45):
                return 5
            elif in_range2(rain, 3.45, 6.0):
                return 6
            elif rain > 6:
                return 7
            else:
                return 0
        elif dsa == 2:
            if in_range(rain, 1.0, 1.45):
                return 3
            elif in_range2(rain, 1.45, 4.45):
                return 4
            elif in_range2(rain, 4.45, 8.0):
                return 5
            elif rain > 8:
                return 6
            else:
                return 0
        elif dsa == 3:
            if in_range(rain, 1.0, 2.45):
                return 3
            elif in_range2(rain, 2.45, 5.0):
                return 4
            elif rain > 5:
                return 5
            else:
                return 0
        elif in_range(dsa, 4.0, 5.0):
            if in_range(rain, 1.0, 2.45):
                return 3
            elif in_range2(rain, 2.45, 8):
                return 4
            elif rain > 8:
                return 5
            else:
                return 0
        elif in_range(dsa, 6.0, 9.0):
            if in_range(rain, 1.0, 4.0):
                return 3
            elif rain > 4:
                return 4
            else:
                return 0
        elif in_range(dsa, 10.0, 14.0):
            if in_range(rain, 1.0, 1.45):
                return 2
            elif in_range2(rain, 1.45, 8.0):
                return 3
            elif rain > 8:
                return 4
            else:
                return 0
        elif dsa > 14:
            if in_range(rain, 1.0, 8.0):
                return 2
            elif rain > 8:
                return 3
            else:
                return 0
        else:
            return 0

def check_bu_cutoff(abu, vt):
    """
    Return True if the blight units (abu) exceed the cutoff for the given variety type.
    """
    # Efficient approach using a dictionary mapping
    thresholds = {"s": 30, "ms": 35, "mr": 40, "r": 45, "hr": 50}
    return abu >= thresholds.get(vt, float('inf'))

def check_fu_cutoff(afu, vt):
    """
    Return True if the fungicide units (afu) exceed the cutoff for the given variety type.
    """
    thresholds = {"s": 15, "ms": 20, "mr": 25, "r": 35, "hr": 45}
    return afu > thresholds.get(vt, float('inf'))

def fungicide_recommendation(df, resistance):
    # Print input for debugging
    print(df)
    print(type(df))
    print(resistance)
    
    # Filter rows where "ID-Location" equals 1 and APP > 0
    df_filtered = df[(df["APP"] > 0)]
    
    # Group by APP and compute:
    #   - n: count of rows in the group minus 1
    #   - date: the first date in the group
    grouped = df_filtered.groupby("APP").agg(
        n=("date", lambda x: len(x) - 1),
        date=("date", "first")
    ).reset_index()
    
    # Create initial fungicide_type: if n > 7 then "Systemic", else "Contact"
    grouped["fungicide_type"] = np.where(grouped["n"] > 7, "Systemic", "Contact")
    
    # Ensure APP is integer
    grouped["APP"] = grouped["APP"].astype(int)
    
    # Rename 'date' to 'Date'
    fungicide_applications = grouped.rename(columns={"date": "Date"})
    
    # Apply modifications based on resistance
    if resistance == "mr":
        # For 'mr': if n < 12 and n > 9 then "Contact", if n >= 12 then "Systemic"
        conditions = [
            (fungicide_applications["n"] < 12) & (fungicide_applications["n"] > 9),
            (fungicide_applications["n"] >= 12)
        ]
        choices = ["Contact", "Systemic"]
        fungicide_applications["fungicide_type"] = np.select(
            conditions, choices, default=fungicide_applications["fungicide_type"]
        )
    
    if resistance in ["r", "hr"]:
        # For 'r' or 'hr': if n < 12 then "Contact", if n >= 12 then "Systemic"
        conditions = [
            (fungicide_applications["n"] < 12),
            (fungicide_applications["n"] >= 12)
        ]
        choices = ["Contact", "Systemic"]
        fungicide_applications["fungicide_type"] = np.select(
            conditions, choices, default=fungicide_applications["fungicide_type"]
        )
    
    if resistance == "s":
        # For susceptible varieties, alternate types:
        # odd-numbered applications get "Systemic", even-numbered get "Contact"
        fungicide_applications = fungicide_applications.sort_values("APP").reset_index(drop=True)
        fungicide_applications["fungicide_type"] = fungicide_applications.index.to_series().apply(
            lambda i: "Systemic" if ((i + 1) % 2 == 1) else "Contact"
        )
    elif resistance == "ms":
        # For moderately susceptible varieties, the first application is "Systemic"
        fungicide_applications.loc[0, "fungicide_type"] = "Systemic"
    else:
        # For other resistance values, assign the first application as "Contact"
        fungicide_applications.loc[0, "fungicide_type"] = "Contact"
    
    # Second override: for 's' or 'ms', set the first application to "Systemic", otherwise "Contact"
    if resistance in ["s", "ms"]:
        fungicide_applications.loc[0, "fungicide_type"] = "Systemic"
    else:
        fungicide_applications.loc[0, "fungicide_type"] = "Contact"
    
    # Finally, rename columns as in the R code:
    fungicide_applications = fungicide_applications.rename(columns={
        "APP": "Application Number",
        "fungicide_type": "Type of Fungicide",
        "n": "Number of Days"
    })
    
    return fungicide_applications