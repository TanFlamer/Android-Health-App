package com.example.myapp.subActivities.listSportData;

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

public class SportDataListAdapter extends ArrayAdapter<SportDataListItem> {

    public SportDataListAdapter(@NonNull Context context, int resource, List<SportDataListItem> sportDataListItemList) {
        super(context, resource, sportDataListItemList);
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        if(currentItemView == null)
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.data_sport_list_item, parent, false);

        SportDataListItem sportDataListItem = getItem(position);

        TextView typeView = currentItemView.findViewById(R.id.sportDataType);
        TextView durationView = currentItemView.findViewById(R.id.sportDataDuration);
        TextView calorieView = currentItemView.findViewById(R.id.sportDataCalorie);

        typeView.setText(sportDataListItem.getType());
        durationView.setText(String.valueOf(sportDataListItem.getDuration()));
        calorieView.setText(String.valueOf(sportDataListItem.getCalorie()));

        return currentItemView;
    }
}
