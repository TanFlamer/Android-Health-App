package com.example.myapp.fragmentsSport.expandableListSport;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.TextView;

import com.example.myapp.R;

import java.util.List;
import java.util.Objects;

public class SportExpandableListAdapter extends BaseExpandableListAdapter {

    private Context context;
    private List<SportListItem> sportListItemList;

    public SportExpandableListAdapter(Context context, List<SportListItem> sportListItemList){
        this.context = context;
        this.sportListItemList = sportListItemList;
    }

    @Override
    public int getGroupCount() {
        return sportListItemList.size();
    }

    @Override
    public int getChildrenCount(int i) {
        return sportListItemList.get(i).getSportData().size();
    }

    @Override
    public Object getGroup(int i) {
        return sportListItemList.get(i).getSportData();
    }

    @Override
    public Object getChild(int i, int i1) {
        return sportListItemList.get(i).getSportData().get(i1);
    }

    @Override
    public long getGroupId(int i) {
        return i;
    }

    @Override
    public long getChildId(int i, int i1) {
        return i1;
    }

    @Override
    public boolean hasStableIds() {
        return true;
    }

    @SuppressLint("InflateParams")
    @Override
    public View getGroupView(int i, boolean b, View view, ViewGroup viewGroup) {
        String sportDate = sportListItemList.get(i).getDate();

        if(view == null)
            view = LayoutInflater.from(context.getApplicationContext()).inflate(R.layout.sport_list_item, null);

        TextView dateView = view.findViewById(R.id.sportDate);
        dateView.setText(sportDate);

        return view;
    }

    @SuppressLint("InflateParams")
    @Override
    public View getChildView(int i, int i1, boolean b, View view, ViewGroup viewGroup) {
        SportData sportData = sportListItemList.get(i).getSportData().get(i1);

        if(view == null)
            view = LayoutInflater.from(context.getApplicationContext()).inflate(R.layout.sport_list_item_data, null);

        TextView nameView = view.findViewById(R.id.sportName);
        TextView durationView = view.findViewById(R.id.sportDuration);
        TextView calorieView = view.findViewById(R.id.sportCalorie);

        nameView.setText(sportData.getName());
        durationView.setText(String.valueOf(sportData.getDuration()));
        calorieView.setText(String.valueOf(sportData.getCalories()));

        return view;
    }

    @Override
    public boolean isChildSelectable(int i, int i1) {
        return true;
    }
}
