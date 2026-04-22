// ============================================================
// THE ONLY FILE YOU EDIT WHEN ADDING / REMOVING A TABLE.
// Both viewer.html and index.html read this list automatically.
//
// To ADD a table:    add a new { section, title, url } entry.
// To REMOVE a table: delete its entry.
// To REORDER:        move entries up or down.
//
// - "section" is the heading it goes under on index.html.
// - The order in this list is the Previous/Next order in the viewer.
// ============================================================

const BASE = "https://dulal-ramesh.github.io/International_Migration_and_Nepal_Civil_War/Paper/Tables/";

const TABLES = [
  // ---------- Summary Tables ----------
  { section: "Summary Tables",
    title: "Table 1 — Descriptive Statistics",
    url: BASE + "Tables_Summary/1.Overall_Summary.html" },
  { section: "Summary Tables",
    title: "Table 2 — Sample Construction",
    url: BASE + "Tables_Summary/2.Sample_Construction.html" },

  // ---------- Balance Tables ----------
  { section: "Balance Tables",
    title: "Table 11a — Balance Table (Treatment Group: 0-17)",
    url: BASE + "Tables_Summary/11a.Balance_Table.html" },
  { section: "Balance Tables",
    title: "Table 11b — Balance Table (Treatment Group: 0-5)",
    url: BASE + "Tables_Summary/11b.Balance_Table.html" },
  { section: "Balance Tables",
    title: "Table 11c — Balance Table (Treatment Group: 6-12)",
    url: BASE + "Tables_Summary/11c.Balance_Table.html" },
  { section: "Balance Tables",
    title: "Table 11d — Balance Table (Treatment Group: 13-17)",
    url: BASE + "Tables_Summary/11d.Balance_Table.html" },

  // ---------- Main Regression Tables ----------
  { section: "Regression Tables",
    title: "Table 13a — Main Result (Intensity: Month of War)",
    url: BASE + "Tables_Main/13a.Main_Regression_MonthsWar.html" },
  { section: "Regression Tables",
    title: "Table 13b — Main Result (Intensity: Number of Casualties)",
    url: BASE + "Tables_Main/13b.Main_Regression_Casualties.html" },
  { section: "Regression Tables",
    title: "Table 13c — Main Result (Binary Intensity: Based on Months of War)",
    url: BASE + "Tables_Main/13c.Main_Regression_BinaryWar.html" },
  { section: "Regression Tables",
    title: "Table 13d — Main Result (Binary Intensity: Based on Casualty Number)",
    url: BASE + "Tables_Main/13d.Main_Regression_BinaryCasualty.html" },

  // ---------- Outcome: International Migrant ----------
  { section: "Progressive Regression — Outcome: International Migrant",
    title: "Table 13a1 — International Migrant (Intensity: Month of War)",
    url: BASE + "Tables_Main/13a1.Regression_IntMigrant_MonthsWar.html" },
  { section: "Progressive Regression — Outcome: International Migrant",
    title: "Table 13a2 — International Migrant (Intensity: Number of Casualties)",
    url: BASE + "Tables_Main/13a2.Regression_IntMigrant_Casualties.html" },
  { section: "Progressive Regression — Outcome: International Migrant",
    title: "Table 13a3 — International Migrant (Binary: Months of War)",
    url: BASE + "Tables_Main/13a3.Regression_IntMigrant_BinaryWar.html" },
  { section: "Progressive Regression — Outcome: International Migrant",
    title: "Table 13a4 — International Migrant (Binary: Casualty Number)",
    url: BASE + "Tables_Main/13a4.Regression_IntMigrant_BinaryCasualty.html" },

  // ---------- Outcome: Currently Abroad ----------
  { section: "Progressive Regression — Outcome: Currently Abroad",
    title: "Table 13b1 — Currently Abroad (Intensity: Month of War)",
    url: BASE + "Tables_Main/13b1.Regression_CurrentlyAbroad_MonthsWar.html" },
  { section: "Progressive Regression — Outcome: Currently Abroad",
    title: "Table 13b2 — Currently Abroad (Intensity: Number of Casualties)",
    url: BASE + "Tables_Main/13b2.Regression_CurrentlyAbroad_Casualties.html" },
  { section: "Progressive Regression — Outcome: Currently Abroad",
    title: "Table 13b3 — Currently Abroad (Binary: Months of War)",
    url: BASE + "Tables_Main/13b3.Regression_CurrentlyAbroad_BinaryWar.html" },
  { section: "Progressive Regression — Outcome: Currently Abroad",
    title: "Table 13b4 — Currently Abroad (Binary: Casualty Number)",
    url: BASE + "Tables_Main/13b4.Regression_CurrentlyAbroad_BinaryCasualty.html" },

  // ---------- Outcome: Return Migrant ----------
  { section: "Progressive Regression — Outcome: Return Migrant",
    title: "Table 13c1 — Return Migrant (Intensity: Month of War)",
    url: BASE + "Tables_Main/13c1.Regression_ReturnMigrant_MonthsWar.html" },
  { section: "Progressive Regression — Outcome: Return Migrant",
    title: "Table 13c2 — Return Migrant (Intensity: Number of Casualties)",
    url: BASE + "Tables_Main/13c2.Regression_ReturnMigrant_Casualties.html" },
  { section: "Progressive Regression — Outcome: Return Migrant",
    title: "Table 13c3 — Return Migrant (Binary: Months of War)",
    url: BASE + "Tables_Main/13c3.Regression_ReturnMigrant_BinaryWar.html" },
  { section: "Progressive Regression — Outcome: Return Migrant",
    title: "Table 13c4 — Return Migrant (Binary: Casualty Number)",
    url: BASE + "Tables_Main/13c4.Regression_ReturnMigrant_BinaryCasualty.html" },

  // ---------- Outcome: Internal Migrant ----------
  { section: "Progressive Regression — Outcome: Internal Migrant",
    title: "Table 13d1 — Internal Migrant (Intensity: Month of War)",
    url: BASE + "Tables_Main/13d1.Regression_InternalMigrant_MonthsWar.html" },
  { section: "Progressive Regression — Outcome: Internal Migrant",
    title: "Table 13d2 — Internal Migrant (Intensity: Number of Casualties)",
    url: BASE + "Tables_Main/13d2.Regression_InternalMigrant_Casualties.html" },
  { section: "Progressive Regression — Outcome: Internal Migrant",
    title: "Table 13d3 — Internal Migrant (Binary: Months of War)",
    url: BASE + "Tables_Main/13d3.Regression_InternalMigrant_BinaryWar.html" },
  { section: "Progressive Regression — Outcome: Internal Migrant",
    title: "Table 13d4 — Internal Migrant (Binary: Casualty Number)",
    url: BASE + "Tables_Main/13d4.Regression_InternalMigrant_BinaryCasualty.html" }
];