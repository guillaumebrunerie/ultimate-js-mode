const App = () => {
	// The span and nav (JSX)
	// The div and Component (normal)
	// The attribute (normal + new line)
	return (
		<div>
			<span a="b"/>
			{array.map(v => {
				if (v.show) {
					return (
						<Component>
							<nav/>
						</Component>
					);
				}
			})}
		</div>
	);
};
