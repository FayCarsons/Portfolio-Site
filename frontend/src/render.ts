interface StatefulWidget<T> {
    state: T;
    render: (state: T) => void;
}

interface StatelessWidget {
    render: () => void;
}

type Widget<T> = StatefulWidget<T> | StatelessWidget;

// @ts-ignore
function render(widgets: Widget<any>[]) {
    function loop() {
        for (let widget of widgets) {
            if ('state' in widget) {
                widget.render(widget.state);
            } else {
                widget.render();
            }
        }
        requestAnimationFrame(loop);
    }
    requestAnimationFrame(loop);
}