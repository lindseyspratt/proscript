% task(ID, Type, Status, Desc, ParentCategory)
% ID names the task
% Type is iso_feature, iso_extra, web_api
% Status is completed, in_progress, not_needed, to_be_done
% Desc is description as list of char codes.
% ParentCategory is the category for the task
%
% task/4 is used to generate content.index_status.template
%            <h2>7.4.2 Directives</h2>
%            <span style="color:purple">Most items implemented</span>
%            <ol>
%                <li>dynamic/1.</li>
%
%   <h2>ParentCategoryDescription</h2
%   <span style="color:{DerivedCategoryStatusColor}"> {DerivedCategoryStatusDescription} </span>
%   <ol>
%       <li>{TaskDescription}</li>
