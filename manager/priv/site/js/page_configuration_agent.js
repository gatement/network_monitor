$(document).ready(function()
{
	$(".agentTest").click(agent_test_click);
	window.ajaxContainer = {}; // store ajax request
});

function agent_test_click(event)
{
	var $agent_name = $(event.target).parent().parent().parent().find(".agentName");
	var $agent_desc = $(event.target).parent().parent().parent().find(".agentDesc");
	var id = $(event.target).parent().parent().parent().find("[name='id']").val();

	$agent_name.text("");
	$agent_desc.text("");

	var opts = {
		lines: 10, // The number of lines to draw
		length: 3, // The length of each line
		width: 2, // The line thickness
		radius: 4, // The radius of the inner circle
		corners: 1, // Corner roundness (0..1)
		rotate: 0, // The rotation offset
		color: '#000', // #rgb or #rrggbb
		speed: 1, // Rounds per second
		trail: 60, // Afterglow percentage
		shadow: false, // Whether to render a shadow
		hwaccel: false, // Whether to use hardware acceleration
		className: 'spinner', // The CSS class to assign to the spinner
		zIndex: 2e9, // The z-index (defaults to 2000000000)
		top: -8, // Top position relative to parent in px
		left: 0 // Left position relative to parent in px
	};
	var name_spinner = new Spinner(opts).spin($agent_name[0]);
	var desc_spinner = new Spinner(opts).spin($agent_desc[0]);

	var get_desc_func = function()
	{
		if(window.ajaxContainer["desc" + id])
		{
			window.ajaxContainer["desc" + id].abort();
		}
		window.ajaxContainer["desc" + id] = $.ajax({
			url: "/configuration/agent/test/desc",
			type: "POST",
			data: { id: id },
			success: function(data, textStatus, jqXHR)
			{
				if(data.success)
				{
					$agent_desc.text(data.data.desc);
				}
				else
				{
					$agent_desc.html("<label style=\"color: Red\">(failed)</label>");
				}
			},
			error: function(jqXHR, textStatus, errorThrown)
			{
				if(textStatus !== "abort")
				{
					$agent_desc.html("<label style=\"color: Red\">(failed)</label>");
				}
			},
			complete: function(jqXHR, textStatus)
			{
				desc_spinner.stop();		
			}
		});
	}
	
	if(window.ajaxContainer["name"+id])
	{
		window.ajaxContainer["name" + id].abort(); 
	}
	window.ajaxContainer["name" + id] = $.ajax({
		url: "/configuration/agent/test/name",
		type: "POST",
		data: { id: id },
		success: function(data, textStatus, jqXHR)
		{
			if(data.success)
			{
				$agent_name.text(data.data.name);
			}
			else
			{
				$agent_name.html("<label style=\"color: Red\">(failed)</label>");
			}
		},
		error: function(jqXHR, textStatus, errorThrown)
		{
			if(textStatus !== "abort")
			{
				$agent_name.html("<label style=\"color: Red\">(failed)</label>");
			}
		},
		complete: function(jqXHR, textStatus)
		{
			name_spinner.stop();
			get_desc_func();			
		}
	});
	
	return false;
}


