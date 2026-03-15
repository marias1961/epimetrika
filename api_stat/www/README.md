# www folder

This folder is for static assets (JavaScript apps, CSS, images).

## Adding JavaScript Calculators

1. Create a subfolder for each JS app (e.g., `www/js_calculator/`)
2. Place your HTML, JS, and CSS files inside
3. Add an iframe in `app.R` to embed it:

```r
nav_panel(title = "JS Calculator",
          tags$iframe(src = "js_calculator/index.html", 
                      width = "100%", 
                      height = "800px",
                      frameborder = "0"))
```

## Example Structure

```
www/
├── README.md
├── js_calculator_1/
│   ├── index.html
│   ├── script.js
│   └── style.css
└── js_calculator_2/
    └── ...
```
