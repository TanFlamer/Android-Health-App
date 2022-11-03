package com.example.myapp.fragmentsSport.listSport;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.example.myapp.R;

import java.util.List;

public class SportListAdapter extends ArrayAdapter<SportListItem> {

    public SportListAdapter(@NonNull Context context, int resource, List<SportListItem> sportListItemList) {
        super(context, resource, sportListItemList);
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.sport_list_item, parent, false);

        SportListItem sportListItem = getItem(position);

        TextView typeView = currentItemView.findViewById(R.id.sportType);
        TextView energyView = currentItemView.findViewById(R.id.sportEnergy);

        typeView.setText(sportListItem.getType());
        energyView.setText(String.valueOf(sportListItem.getCaloriePerMinute()));

        return currentItemView;
    }
}
